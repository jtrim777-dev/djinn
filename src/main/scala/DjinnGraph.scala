package com.github.jtrim777.djinn

import scala.annotation.tailrec

import DjinnGraph._
import cats.effect.IO
import defns.{ModuleInstance, Version}

case class DjinnGraph(nodes: Array[Node], edges: DjinnGraph.EdgeSet, resolver: ModuleResolver) {
  def find(moduleID: String): Option[Node] = nodes.find(_.module.id == moduleID)

  def regrade(moduleID: String, newVersion: Version): Operation[(DjinnGraph, Int)] = {
    val candidates = idToIndex(moduleID)

    def attempt(curi: Int): Operation[DjinnGraph] = {
      if (nodes(curi).module.version.compatibleWith(newVersion).isDefined) {
        IO.pure(Success(this, Some(s"The module $moduleID already had version $newVersion")))
      } else {
        val depMods = dependsOn(moduleID)
        val relDeps = depMods.flatMap(_.deps.find(_.id == moduleID))

        val compat = Version.intersect(newVersion +: relDeps.map(_.versionSpec))

        compat match {
          case Some(goodVersion) =>
            val proc = for {
              inst <- resolver.resolve(moduleID, goodVersion).map(_.head)
              updateSelf = this.copy(nodes = nodes.updated(curi, Node(inst, goodVersion)))
              newSelf <- updateSelf.addMany(inst.deps.map(d => (d.id, d.versionSpec)), Some(curi))
            } yield newSelf

            proc.extendOpError(s"Cannot change $moduleID to version ${newVersion.toString}")
          case None =>
            val incom = depMods.map { mid =>
              val d = mid.relevantDependency(moduleID).get
              s"${mid.id} requires ${d.toString}"
            }.mkString("\n")

            IO.pure(Failure[DjinnGraph](s"Cannot change $moduleID to version ${newVersion.toString}",
              s"The following modules have conflicting dependencies: \n$incom"))
        }
      }
    }

    if (candidates.isEmpty) {
      IO.pure(Failure[(DjinnGraph, Int)](s"Cannot change $moduleID to version ${newVersion.toString}",
        s"$moduleID does not exist in the graph"))
    } else {
      attempt(candidates.get).map(_.map(g => (g, candidates.get)))
    }
  }

  def add(moduleId: String, version: Version, parent: Option[Int] = None, allowMerge: Boolean = true): Operation[DjinnGraph] = {

    val op = if (find(moduleId).nonEmpty && allowMerge) {
      regrade(moduleId, version).flatMap {
        case Success((g, u), message) =>
          parent match {
            case Some(value) =>
              IO(g.link(value, u))
            case None => IO.pure(Success(g, message))
          }
        case Failure(action, cause, parentActions) =>
          IO.pure(Failure[DjinnGraph](action, cause, parentActions))
      }
    } else {
      val i = nodes.length

      for {
        insts <- resolver.resolve(moduleId, version)
        rez <- firstSuccess[ModuleInstance, DjinnGraph](insts) { inst =>
          val updateSelf = this.forceAddNode(Node(inst, version))
          for {
            linkedSelf <- parent match {
              case Some(value) => IO(updateSelf.link(value, i)).oet
              case None => IO.pure(updateSelf)
            }
            newSelf <- linkedSelf.addMany(inst.deps.map(d => (d.id, d.versionSpec)), Some(i))
          } yield newSelf
        }
      } yield rez
    }

    IO(println(s"Attempting to add $moduleId@$version as dependency of " +
      s"${parent.map(i => nodes(i).module.toString).getOrElse("")}")) *> op
  }

  def addMany(news: Seq[(String, Version)], parent: Option[Int] = None): Operation[DjinnGraph] = {
    val bc: Operation[DjinnGraph] = IO.pure(Success(this))

    news.foldLeft(bc) { case (proc, (newID, newVersion)) =>
      proc.flatMapSuccess((g, _) => g.add(newID, newVersion, parent))
    }
  }

  private def link(modA: Int, modB: Int): OpResult[DjinnGraph] = {
    if (edges(modA).contains(modB)) {
      Success(this)
    } else if (dfs(modB, modA)) {
      val n1 = nodes(modB).module
      val n2 = nodes(modA).module
      Failure(s"Cannot mark ${n1.id}@${n1.version} as a dependency of ${n2.id}@${n2.version}",
        "Cycle found in dependency graph")
    } else Success(forceAddEdge(modA, modB))
  }

  private def forceAddEdge(src: Int, dst: Int): DjinnGraph = {
    this.copy(edges = edges.updated(src, dst :: edges(src)))
  }

  private def forceAddNode(data: Node): DjinnGraph =
    this.copy(nodes = nodes :+ data, edges = edges :+ List.empty)

  def idToIndex(id: String): Option[Int] = nodes.zipWithIndex.find(_._1.module.id == id).map(_._2)

  @tailrec
  final def dfs(src: Int, target: Int, seen: List[Int] = List.empty, ready: List[Int] = List.empty): Boolean = {
    val connected = edges(src)

    connected.nonEmpty && connected.contains(target) || {
      val nseen = src :: seen
      val nready = ready ++ connected.filterNot(seen.contains)

      nready.nonEmpty && dfs(nready.head, target, nseen, nready.tail)
    }
  }

  def dependsOn(mod: String): Seq[ModuleInstance] = nodes.filter { node =>
    node.module.deps.exists(_.id == mod)
  }.map(_.module)

  private def firstSuccess[I, O](inp: Seq[I], errors: Seq[Failure[O]] = Seq.empty)
                                (f: I => Operation[O]): Operation[O] = {
    if (inp.isEmpty) {
      val bleh = errors.map(_.getMessage.split('\n').map(s => "  " + s).mkString("\n")).mkString("\n")
      val fail = Failure[O]("Could not satisfy constraints", s"All attempts failed\n$bleh")
      IO.pure(fail)
    } else {
      f(inp.head).flatMap {
        case fl: Failure[O] => firstSuccess(inp.tail, errors :+ fl)(f)
        case s: Success[O] => IO.pure(s)
      }
    }
  }
}

object DjinnGraph {
  def apply(resolver: ModuleResolver): DjinnGraph = {
    new DjinnGraph(Array.empty, Array.empty, resolver)
  }

  sealed trait OpResult[T] {
    def map[R](f: T => R): OpResult[R]

    def isSuccess: Boolean
  }

  case class Failure[T](action: String, cause: String, parentActions: List[String] = List.empty) extends Error with OpResult[T] {
    def extend(act: String): Failure[T] = this.copy(parentActions = act :: parentActions)

    override def map[R](f: T => R): OpResult[R] = Failure[R](action, cause, parentActions)

    override def isSuccess: Boolean = false

    override def getMessage: String = {
      if (parentActions.isEmpty) {
        s"$action: $cause"
      } else {
        val parents = parentActions.reverse.map(s => "  at " + s).mkString("\n")
        s"$action: $cause\n$parents"
      }
    }
  }

  case class Success[T](result: T, message: Option[String] = None) extends OpResult[T] {
    override def map[R](f: T => R): OpResult[R] = Success(f(result), message)

    override def isSuccess: Boolean = true
  }

  type Operation[T] = IO[OpResult[T]]

  implicit class OpOps[T](op: Operation[T]) {
    def catchErrors: Operation[T] = op.attempt.map {
      case Right(value) => value
      case Left(value) => Failure("Error performing action", value.toString)
    }

    def extendOpError(action: String): Operation[T] = op.catchErrors.map {
      case f: Failure[T] => f.extend(action)
      case s => s
    }

    def mapSuccess[R](f: (T, Option[String]) => OpResult[R]): Operation[R] = op.map {
      case f: Failure[T] => Failure[R](f.action, f.cause, f.parentActions)
      case Success(rez, msg) => f(rez, msg)
    }

    def flatMapSuccess[R](f: (T, Option[String]) => Operation[R]): Operation[R] = op.flatMap {
      case f: Failure[T] => IO.pure(Failure[R](f.action, f.cause, f.parentActions))
      case Success(rez, msg) => f(rez, msg)
    }

    def mapFailure(f: () => OpResult[T]): Operation[T] = op.map {
      case _: Failure[T] => f()
      case s@Success(_, _) => s
    }

    def flatMapFailure(f: () => Operation[T]): Operation[T] = op.flatMap {
      case _: Failure[T] => f()
      case s@Success(_, _) => IO.pure(s)
    }

    def oet: IO[T] = op.flatMap {
      case f: Failure[T] => IO.raiseError(f)
      case Success(result, message) => IO.pure(result)
    }
  }

  case class Node(module: ModuleInstance, valid: Version)

  type EdgeSet = Array[List[Int]]
}
