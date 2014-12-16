package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop0 { self =>
  def check: Boolean
  def &&(p: Prop0): Prop0 = new Prop0 {
    override def check: Boolean = self.check && p.check
  }
}

trait Prop1 { self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop1): Prop1 =
    new Prop1 {
      override def check = self.check match {
        case Right(_) => p.check
        case left@Left(e) => left
      }
    }
}

case class Prop(run: (TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (tc,rng) =>
    run(tc, rng) match {
      case Passed => p.run(tc, rng)
      case f@Falsified(e, c) => f
    }
  }
  // Eager implementation wasteful because it invokes second property when it might not need to
  //  Prop { (tc, rng) =>
  //    val r1 = run.apply(tc, rng)
  //    val r2 = p.run.apply(tc, rng)
  //    if (!r1.isFalsified && !r2.isFalsified) Passed
  //    else if (r1.isFalsified) r1
  //    else r2
  //  }

  def ||(p: Prop): Prop = Prop { (tc,rng) =>
    run(tc, rng) match {
      case ok@Passed => ok
      case Falsified(e, c) => p.tag(e).run(tc, rng)
    }
  }
  // Eager implementation wasteful because it invokes second property when it might not need to
  //  Prop { (tc, rng) =>
  //    val r1 = run.apply(tc, rng)
  //    val r2 = p.run.apply(tc, rng)
  //    (r1, r2) match {
  //      case (Falsified(f1, s1), Falsified(f2, s2)) => p.tag(f2).run.apply(tc, rng)
  //      case _ => Passed
  //    }
  //  }

  def tag(msg: String): Prop = Prop { (tc,rng) =>
    run(tc, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object ListProps {
  // Exercise 8.14: Prop for List.sorted
  val smallInt = Gen.choose(-10,10)
  lazy val intListGen: Gen[List[Int]] = listOfN(3, smallInt)
  lazy val sortedProp: Prop =
      Prop.forAll(intListGen) { l: List[Int] =>
        isOrdered(l.sorted) == true
      }

  def isOrdered(l: List[Int]) =
    l.foldLeft((true, None:Option[Int]))((x,y) =>
      (x._1 && x._2.map(_ <= y).getOrElse(true), Some(y)))._1

  // Exercise 8.14: Prop for List.takeWhile
  lazy val takeWhileProp: Prop = {
    val f = (_: Int) <= 0
    val p1 = Prop.forAll(intListGen) { l: List[Int] =>
      l.takeWhile(f).forall(f) == true
    }
    val p2: Prop = Prop.forAll(intListGen) { l: List[Int] =>
      l == l.takeWhile(f) ++ l.dropWhile(f)
    }
    p1 && p2
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.double).map(i => start + (i * (stopExclusive - start)).toInt))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeLessThan(2)).map(i => if (i == 1) true else false))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  // here is an example on how to combine generators in a for-comprehension
  def option[A](gen: Gen[A]): Gen[Option[A]] =
    for {
        b <- Gen.boolean
        a <- gen
      } yield if (b) Some(a) else None

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def stringN(n: Int): Gen[String] = ???

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)
    // One possible way, rather imperative
    // for {
    //   b <- Gen.boolean
    //   a1 <- g1
    //   a2 <- g2
    // } yield {
    //   if (b) a1 else a2
    // }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val prob = g1._2.abs / (g1._2.abs + g2._2.abs)
    for {
      d <- double
      a1 <- g1._1
      a2 <- g2._1
    } yield {
      if (d < prob) a1 else a2
    }
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen((i: Int) => listOfN(i, g))
    // Original solution
    // SGen((i: Int) => g.map(a => List.fill(i)(a)))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen((i: Int) => listOfN(i max 1, g))

  lazy val parInt: Gen[Par[Int]] =
    // Randomly passes test :|
    choose(-100,100).listOfN(choose(0,20)).map(l =>
      l.foldLeft(Par.unit(0))((p,i) =>
        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))
    // Does not pass test:
    // choose(-100, 100).map(i => Par.lazyUnit(i))
    //
    // Does not pass test:
    // Gen(State(RNG.int).map(i => Par.lazyUnit(i)))
}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfN(size: Int): Gen[List[A]] = ???

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

  def unsized: SGen[A] = SGen((i: Int) => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize.andThen(g => g.map(f)))
    // Original solution:
    // SGen((i: Int) => forSize(i).map(f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize.andThen(g => g.flatMap(f)))
    // Original solution:
    // SGen((i: Int) => forSize(i).flatMap(f))

  def **[B](s2: SGen[B]): SGen[(A,B)] = SGen((i: Int) => apply(i).**(s2(i)))
}
