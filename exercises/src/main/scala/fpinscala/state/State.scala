package fpinscala.state

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n == Int.MinValue) (0, rng2)
    else (Math.abs(n), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nn, rng2) = nonNegativeInt(rng)
    // Between 0 and 1, but not including 1
    // Calling toDouble is important
    // You add one to max value so that even max value would not return 1.0
    (nn / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intRng, rng2) = rng.nextInt
    val (doubleRng, rng3) = double(rng2)
    ((intRng, doubleRng), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleRng, rng2) = double(rng)
    val (intRng, rng3) = rng2.nextInt
    ((doubleRng, intRng), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (doubleRng, rng2) = double(rng)
    val (doubleRng2, rng3) = double(rng2)
    val (doubleRng3, rng4) = double(rng3)
    ((doubleRng, doubleRng2, doubleRng3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec def loop(i: Int, curr: (List[Int], RNG)): (List[Int], RNG) = {
      if (count == i) curr
      else {
        val (intN, rngN) = curr._2.nextInt
        loop(i + 1, (intN :: curr._1, rngN))
      }
    }
    loop(0, (List(), rng))
  }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((x, acc) => map2(x, acc)((a, l) => a :: l))

    // Original solution
    // rng => {
    //   val ret = fs.foldRight(List.empty[A], rng) { case (x, acc) =>
    //     val list = acc._1
    //     val rngx = acc._2
    //     val a = x(rngx)
    //     (a._1 :: list, a._2)
    //   }
    //   ret
    // }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val a = f(rng)
      g(a._1)(a._2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State((s: S) => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State((s: S) => {
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f(a, b), s3)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State((s: S) => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List.empty[A]))((x, acc) => x.map2(acc)((a, l) => a :: l))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}