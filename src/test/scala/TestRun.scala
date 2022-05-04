package com.github.jtrim777.djinn
import java.nio.file.{Files, Paths}
import scala.util.Try

import cats.effect.{ExitCode, IO, IOApp}
import defns.{Dependency, ModuleInstance, Version}
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import util.{DOTGen, SEOps}
import DjinnGraph.OpOps

object TestRun extends IOApp {
  case class DumbResolver(modules: Seq[ModuleInstance]) extends ModuleResolver {
    override def resolve(id: String, versionSpec: Version): IO[Seq[ModuleInstance]] = {
      IO {
        modules.filter(m => m.id == id && m.version.compatibleWith(versionSpec).isDefined)
      }.flatMap { s => if (s.isEmpty) {
        IO.raiseError(new IllegalArgumentException(s"No such module $id with version $versionSpec"))
      } else IO.pure(s)}
    }
  }

  def decodingError[T](msg: String): Decoder.Result[T] = Left(DecodingFailure(msg, List.empty))

  implicit val vdec: Decoder[Version] = { (c:HCursor) =>
    c.as[String].flatMap { raw =>
      if (raw.contains('.')) {
        val (head, tag) = if (raw.contains('-')) {
          val pts = raw.split('-')
          (pts.head, Some(pts.tail.mkString("-")))
        } else (raw, None)

        val pts = head.split('.')
        if (pts.length == 3) {
          if (pts.exists(_.contains(':'))) {
            val ranges = pts.map { rr =>
              if (rr.contains(':')) {
                val rangePts = rr.split(':')
                if (rangePts.length == 2) {
                  val s = if (rangePts.head.isEmpty) Right(0) else {
                    rangePts.head.toIntOption match {
                      case Some(value) => Right(value)
                      case None => decodingError(s"Non-numeric value in semantic version range $raw")
                    }
                  }
                  val e = if (rangePts(1).isEmpty) Right(Int.MaxValue) else {
                    rangePts(1).toIntOption match {
                      case Some(value) => Right(value)
                      case None => decodingError(s"Non-numeric value in semantic version range $raw")
                    }
                  }

                  for {
                    ss <- s
                    ee <- e
                  } yield ss.to(ee)
                } else decodingError(s"Invalid semantic version range $raw")
              } else {
                (rr.toIntOption match {
                  case Some(value) => Right(value)
                  case None => decodingError(s"Non-numeric value in semantic version range $raw")
                }).map(i => i.to(i))
              }
            }.toSeq.sequence

            ranges.map { rranges =>
              Version.SemanticRange(rranges.head, rranges(1), rranges(2), tag)
            }
          } else {
            val items = pts.map { s =>
              s.toIntOption match {
                case Some(value) if value >= 0 => Right(value)
                case None => decodingError(s"Non-numeric value in semantic version $raw")
                case _ => decodingError(s"Invalid numeric value in semantic version $raw")
              }
            }.toSeq.sequence

            items.map { ritems =>
              Version.Semantic(ritems.head, ritems(1), ritems(2), tag)
            }
          }
        } else decodingError(s"Invalid semantic version $raw")
      } else Right(Version.Hash(raw))
    }
  }

  implicit val ddec: Decoder[Dependency] = { (c:HCursor) =>
    val raw = c.as[String]

    raw.flatMap { rawStr =>
      if (rawStr.contains('@')) {
        val pts = rawStr.split('@')
        val id = pts.head
        val vspec = pts.tail.mkString("@")
        vdec.decodeJson(Json.fromString(vspec)).map(v => Dependency(id, v))
      } else {
        Right(Dependency(rawStr, Version.MatchAny))
      }
    }
  }

  implicit val mdec: Decoder[ModuleInstance] = { (c:HCursor) =>
    for {
      name <- c.downField("name").as[String]
      vz <- c.downField("version").as[Option[Version]]
        .map(_.getOrElse(Version.Init))
        .flatMap {
          case c:Version.Concrete => Right(c)
          case o => Left(DecodingFailure(s"Version $o is not concrete", List.empty))
        }
      deps <- c.downField("deps").as[Option[List[Dependency]]]
        .map(_.getOrElse(List.empty))
    } yield ModuleInstance(name, vz, deps)
  }

  def parseModules(text: String): IO[Seq[ModuleInstance]] = {
    IO(io.circe.parser.parse(text)).flatMap {
      case Right(value) => IO.pure(value)
      case Left(value) => IO.raiseError(value)
    }.flatMap { j =>
      j.as[Seq[ModuleInstance]] match {
        case Right(value) => IO.pure(value)
        case Left(err) => IO.raiseError(err)
      }
    }
  }

  val pathHead = "/Users/jake/Documents/Programming/Random/djinn"

  val prog: IO[Unit] = for {
    modBytes <- IO(Files.readAllBytes(Paths.get(s"$pathHead/src/test/resources/wversions.json")))
    modText = new String(modBytes)
    modules <- parseModules(modText)
    base = DjinnGraph(DumbResolver(modules))
    g2 <- base.add("device_drivers.opentrons:opentrons-lib", Version.MatchAny)
    _ <- g2 match {
      case f:DjinnGraph.Failure[DjinnGraph] => IO.println(f.getMessage)
      case DjinnGraph.Success(result, _) =>
        DOTGen.saveAsDotFile(result, Paths.get(s"$pathHead/src/test/resources/wversions.dot"))
    }
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = prog.map(_ => ExitCode.Success)
}
