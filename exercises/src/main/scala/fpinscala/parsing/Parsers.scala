package fpinscala.parsing

import java.util.regex.Pattern

import scala.util.matching.Regex

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.testing.Prop.forAll

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] // 149, 163

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] // 149, 156
  implicit def string(s: String): Parser[String] // 149
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p) // 150
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = // 150
    ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = // 150, 155
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))((a, l) => a :: l)

  def many[A](p: Parser[A]): Parser[List[A]] = // 152, 155
    map2(p, many(p))(_ :: _) or succeed(List())

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = // 152
    flatMap(a)(a => succeed(f(a)))
    // flatMap(a)(f andThen succeed)

  def char(c: Char): Parser[Char] = // 153
    string(c.toString) map ((_: String).charAt(0))

  def succeed[A](a: A): Parser[A] // 153

  def slice[A](p: Parser[A]): Parser[String] // 154

  def many1[A](p: Parser[A]): Parser[List[A]] = // 154
    map2(p, many(p))(_ :: _)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = // 154, 156, 157
    flatMap(p)(a => map(p2)(b => (a, b)))
    // "Missing parameter type" compilation error
    // p.flatMap(a => p2.map(b => (a, b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = // 157
    product(p, p2).map(f.tupled)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] // 157

  implicit def regex(r: Regex): Parser[String] // 157

  def label[A](msg: String)(p: Parser[A]): Parser[A] // 161

  def scope[A](msg: String)(p: Parser[A]): Parser[A] // 162

  def attempt[A](p: Parser[A]): Parser[A] // 164

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2) // 150
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2) // 150
    def map[A1 >: A, B](f: A1 => B): Parser[B] = self.map(p)(f) // 152
    def many[B >: A]: Parser[List[B]] = self.many(p) // 152
    def slice: Parser[String] = self.slice(p) // 154
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2) // 154
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2) // 154
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f) // 157
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }

  // JSON parser methods
  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = map(skipL(string("\""), thru("\"")))(s => s.dropRight(1))

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    map(doubleString)(_.toDouble) label "double literal"

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s)== run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, map(p)(a => a))(in)
  }

  object Exercises {
    def map2ViaProduct[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = // 154
      // Solution converting a 2 parameter function into a tuple (other tried solutions fail to compile)
      p.product(p2).map(f.tupled)

    def csListOfN[A](p: Parser[A]): Parser[List[A]] = // 157
      regex("[0-9_]*".r).flatMap(d => listOfN(d.toInt, p))
  }
}

// 161
case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

// 163
case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError = // 167
    copy(stack = (loc,msg) :: stack)
  def label[A](s: String): ParseError = // 168
    ParseError(latestLoc.map((_, s)).toList)
  def latestLoc: Option[Location] =
    latest map (_._1)
  def latest: Option[(Location, String)] =
    stack.lastOption
}