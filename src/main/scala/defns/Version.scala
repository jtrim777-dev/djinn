package com.github.jtrim777.djinn
package defns

import scala.annotation.tailrec

import util._

sealed trait Version {
  def compatibleWith(other: Version): Option[Version.Concrete]
}

object Version {
  val MatchAny: SemanticRange =
    SemanticRange(0.to(Int.MaxValue), 0.to(Int.MaxValue), 0.to(Int.MaxValue), None)

  val Init: Semantic = Semantic(0,0,1,None)

  sealed trait Concrete extends Version

  case class Semantic(major: Int, minor: Int, build: Int, tag: Option[String]) extends Concrete {
    override def compatibleWith(other: Version): Option[Version.Concrete] = other match {
      case s:Semantic => Option.when(s == this)(this)
      case r:SemanticRange => Option.when(r.includes(this))(this)
      case _ => None
    }

    override def toString: String = {
      s"$major.$minor.$build${tag.map(s => "-"+s).getOrElse("")}"
    }
  }

  case class SemanticRange(major: Range.Inclusive, minor: Range.Inclusive, build: Range.Inclusive,
                           tag: Option[String]) extends Version {


    override def compatibleWith(other: Version): Option[Version.Concrete] = other match {
      case s:Semantic => Option.when(this.includes(s))(s)
      case r:SemanticRange => this.overlaps(r)
      case _ => None
    }

    def includes(sem: Semantic): Boolean = {
      major.contains(sem.major) &&
        minor.contains(sem.minor) &&
        build.contains(sem.build) &&
        tag.forall(sem.tag.contains)
    }

    def overlaps(other: SemanticRange): Option[Semantic] = {
      val high = major.smartIntersect(other.major)
      val med = minor.smartIntersect(other.minor)
      val low = build.smartIntersect(other.build)

      high flatMap { vh =>
        med flatMap { vm =>
          low flatMap { vl =>
            if (tag.forall(other.tag.contains)) Some(Semantic(vh.end, vm.end, vl.end, tag)) else None
          }
        }
      }
    }

    def highest: Semantic = Semantic(major.end, minor.end, build.end, tag)

    override def toString: String = {
      def rangeToDesc(r:Range.Inclusive): String = {
        val s = if (r.start <= 0) "" else r.start.toString
        val e = if (r.end == Int.MaxValue) "" else r.end.toString

        if (r.start == r.end) {
          r.start.toString
        } else s"$s:$e"
      }

      if (this == MatchAny) {
        "any"
      } else {
        s"${rangeToDesc(major)}.${rangeToDesc(minor)}.${rangeToDesc(build)}${tag.map(s => "-"+s).getOrElse("")}"
      }
    }
  }

  case class Hash(value: String) extends Concrete {
    override def compatibleWith(other: Version): Option[Version.Concrete] = other match {
      case Hash(value) => Option.when(value == this.value)(this)
      case _ => None
    }

    override def toString: String = value
  }

  @tailrec
  final def intersect(opts: Seq[Version], curr: Option[Version] = None): Option[Version] = {
    if (opts.isEmpty) {
      curr
    } else {
      val test = opts.head
      val next = curr.orElse(Some(test)).flatMap(c => narrow(c, test))

      if (next.isEmpty) None else intersect(opts.tail, next)
    }
  }

  def narrow(left: Version, right: Version): Option[Version] = {
    if (left.compatibleWith(right).isEmpty) None else {
      left match {
        case concrete: Concrete => Some(concrete)
        case lr:SemanticRange => right match {
          case rconc: Concrete => Some(rconc)
          case rr:SemanticRange => lr.overlaps(rr)
        }
      }
    }
  }
}
