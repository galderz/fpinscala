package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List.empty[A])((x, acc) => x :: acc)

  def take(n: Int): Stream[A] = {
    def take(n: Int, s: Stream[A], acc: Stream[A]): Stream[A] = {
      if (n <= 0) acc
      else {
        s match {
          case Empty => empty
          case Cons(h, t) =>
            cons(h(), take(n - 1, t(), acc)) // appends by adding the head first, then recurse with the rest, but not tail recursive
            // take(n - 1, t(), cons(h(), acc)) <- tail recursive but it prepends :(
        }
      }
    }

    take(n, this, empty)
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, 0)) { case (s, i) =>
      s match {
        case Cons(h, t) if i != n =>  Some(h(), (t(), i + 1))
        case _ => None
      }
    }

  def drop(n: Int): Stream[A] = {
    @tailrec def drop(i: Int, s: Stream[A]): Stream[A] = {
      if (n == i) s
      else {
        s match {
          case Empty => empty
          case Cons(h, t) => drop(i + 1, t())
        }
      }
    }

    drop(0, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def takeWhile(s: Stream[A], acc: Stream[A]): Stream[A] = {
      s match {
        case Empty => empty
        case Cons(h, t) =>
          lazy val head = h()
          if (p(head)) cons(head, takeWhile(t(), acc)) else acc
      }
    }

    takeWhile(this, empty)

    // Alternative implementation, neater:
    //    this match {
    //      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    //      case _ => empty
    //    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => p(x) && acc)

  // Important clarification on foldRight vs foldLeft: http://www.manning-sandbox.com/message.jspa?messageID=135047
  // foldRight allows for early termination since it can opt not to inspect the right argument
  // foldLeft = (z /: List(a, b, c)) (op) equals op(op(op(z, a), b), c)
  // foldRight = (List(a, b, c) :\ z) (op) equals op(a, op(b, op(c, z)))
  //                                              | if op doesn't evaluate the second part, the recursion never occurs, that's how it ends early
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else empty)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((h, _) => Some(h))
    // Initial solution, more complex but works :)
    //    foldRight(Option.empty[A]) {
    //      (x, acc) =>
    //        println(x)
    //        acc match {
    //          case None => Some(x)
    //          case Some(a) => None // Doesn't matter what you return here! Anything except acc, which will result in recursion continuing
    //        }
    //    }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h.apply()), t()))
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else acc)

  def append[B>:A](other: Stream[B]): Stream[B] =
    foldRight(other)((x, acc) => cons(x, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, acc) => f(x).append(acc))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)) { case (str1, str2) =>
      (str1, str2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) { case (str1, str2) =>
      (str1, str2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case (Cons(h1, t1), Empty) => Some(((Some(h1()), Option.empty[B]), (t1(), empty[B])))
        case (Empty, Cons(h2, t2)) => Some(((Option.empty[A], Some(h2())), (empty[A], t2())))
        case _ => None
      }
    }
  } // Ignore IDE error

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2 != None).forAll {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }
  }

  def tails: Stream[Stream[A]] = sys.error("todo using unfold")

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fib1(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  def lazyFibs1(n: Int): Stream[Int] = Stream.cons(fib1(n), lazyFibs1(n + 1))

                                                              // (0,   lazyFibs2(1, 0 + 1))
                                                              // (1,   lazyFibs2(1, 1 + 1))
                                                              // (1,   lazyFibs2(2, 1 + 2))
                                                              // (2,   lazyFibs2(3, 2 + 3))
                                                              // (3,   lazyFibs2(5, 3 + 5))
                                                              // (5,   lazyFibs2(8, 5 + 8))
                                                              // (8,   lazyFibs2(13, 8 + 13))
                                                              // (13,  lazyFibs2(21, 13 + 31))
  def lazyFibs2(fib0: Int, fib1: Int): Stream[Int] = Stream.cons(fib0, lazyFibs2(fib1, fib0 + fib1))

  lazy val fibs: Stream[Int] = lazyFibs2(0, 1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((x) => Stream.cons(x._1, unfold(x._2)(f))).getOrElse(empty)

  lazy val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: Stream[Int] =
    unfold(1)(x => Some(1, 1))
}