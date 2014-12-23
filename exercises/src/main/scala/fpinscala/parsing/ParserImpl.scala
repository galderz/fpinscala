package fpinscala.parsing

import scala.util.matching.Regex

object ParserTypes { // 167
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match { // 168
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match { // 169
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match { // 170
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match { // 170
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing] // 169
}

object ParserImpl extends Parsers[ParserTypes.Parser] {

  import ParserTypes._

  override def run[A](p: Parser[A])(input: String): Either[ParseError,A] = // 149, 163, 170
    p(Location(input)) match {
      case Success(a, n) => Right(a)
      case Failure(pe, committed) => Left(pe)
    }

  override implicit def string(s: String): Parser[String] = { // 149, 167
    val msg = "'" + s + "'"
    (l: Location) => {
      val i = firstNonmatchingIndex(l.input, s, l.offset)
      if (i == -1) // they matched
        Success(s, s.length)
      else
        Failure(l.advanceBy(i).toError(msg), i != 0)
    }
  }

  override implicit def regex(r: Regex): Parser[String] = { // 157, 167
    val msg = "regex " + r
    l => r.findPrefixOf(l.input) match {
      case None => Failure(l.toError(msg), false)
      case Some(m) => Success(m, m.length)
    }
  }

  override def slice[A](p: Parser[A]): Parser[String] = // 154, 167
    l => p(l) match {
      case Success(s, n) => Success(l.input.substring(l.offset, l.offset + n), n)
      case f@Failure(_, _) => f
    }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = // 161
    s => p(s).mapError(_.label(msg)) // 168

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = // 162
    loc => p(loc).mapError(_.push(loc, msg)) // 168

  override def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = // 157
    s => p(s) match { // 169
      case Success(a, n) => f(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case e @ Failure(_, _) => e
    }

  override def attempt[A](p: Parser[A]): Parser[A] = // 164
    loc => p(loc).uncommit // 169

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = // 149, 156
    s => s1(s) match { // 169
      case Failure(e, false) => s2(s)
      case r => r
    }

  override def succeed[A](a: A): Parser[A] = // 153, 167
    l => Success(a, 0)

  /** Returns -1 if s.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s.length. */
  def firstNonmatchingIndex(s: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s.length && i < s2.length) {
      if (s.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s.length-offset >= s2.length) -1
    else s.length-offset
  }

}