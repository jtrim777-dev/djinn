package com.github.jtrim777.djinn
package util

import java.nio.file.{Files, Path}

import cats.effect.IO
import io.circe.syntax._

object DOTGen {
  def generateNodeDefns(graph: DjinnGraph): Seq[String] = {
    graph.nodes.zipWithIndex.map { case (node, i) =>
      val name = s"${node.module.id}@${node.module.version.toString}"
      name.asJson.noSpaces
    }
  }

  def generateEdgeStmts(graph: DjinnGraph): Seq[String] = {
    graph.edges.zipWithIndex.flatMap { case (conns, source) =>
      val sname = s"${graph.nodes(source).module.id}@${graph.nodes(source).module.version.toString}"
      conns.map { i =>
        val dname = s"${graph.nodes(i).module.id}@${graph.nodes(i).module.version.toString}"
        s"${sname.asJson.noSpaces} -> ${dname.asJson.noSpaces}"
      }
    }
  }

  def makeDot(graph: DjinnGraph): String = {
    val nodes = generateNodeDefns(graph)
      .map(s => "  " + s)
      .mkString(";\n")
    val edges = generateEdgeStmts(graph)
      .map(s => "  " + s)
      .mkString(";\n")

    "digraph {\n" + nodes + ";\n\n" + edges + ";\n}\n"
  }

  def saveAsDotFile(graph: DjinnGraph, file: Path): IO[Unit] = {
    val text = makeDot(graph)

    IO(Files.write(file, text.getBytes))
  }
}
