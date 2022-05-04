package com.github.jtrim777.djinn

import cats.MonadThrow
import cats.effect.IO

package object util {
  implicit class RangeOps(range: Range.Inclusive) {
    def smartIntersect(other: Range.Inclusive): Option[Range.Inclusive] = {
      if (!range.contains(other.start) && !range.contains(other.end)) {
        None
      } else {
        val start = if (range.contains(other.start)) other.start else range.start
        val end = if (range.contains(other.end)) other.end else range.end

        Some(start.to(end))
      }
    }
  }

  private val IOX = MonadThrow[IO]
  implicit class IOOps[T](io: IO[T]) {
    def adaptError(pf: PartialFunction[Throwable, Throwable]): IO[T] = {
      IOX.adaptError(io)(pf)
    }
  }

  private def seqEithers[L, R](seq: Seq[Either[L, R]]): Either[L, Seq[R]] = if (seq.isEmpty) {
    Right(Seq.empty)
  } else {
    seq.head match {
      case Right(value) =>
        val rem = seqEithers(seq.tail)
        rem match {
          case Right(lst) => Right(value +: lst)
          case Left(err) => Left(err)
        }
      case Left(err) => Left(err)
    }
  }

  implicit class SEOps[L, R](seq: Seq[Either[L, R]]) {
    def sequence: Either[L, Seq[R]] = seqEithers(seq)
  }
}
