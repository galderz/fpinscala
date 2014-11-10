package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(e) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    // flatMap(aa => b.map(bb => f(aa, bb)))
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = sys.error("todo")

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def map2[A,B,C,E](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] =
    a.flatMap(x => b.map(y => f(x, y)))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    // es.foldLeft[Either[E, List[A]]](Right(Nil))((acc, x) => acc.flatMap(l => x.map(l :+ _)))
    es.foldLeft[Either[E, List[A]]](Right(Nil))((acc, x) => map2(acc, x)(_ :+ _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft[Either[E, List[B]]](Right(Nil))((acc, x) => map2(acc, f(x))(_ :+ _))

}