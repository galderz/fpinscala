package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[_]): Int = {
    def size(t: Tree[_], acc: Int): Int = {
      t match {
        case Leaf(a) => acc + 1
        case Branch(l, r) =>
          1 + size(l, acc) + size(r, acc)
      }
    }
    size(t, 0)
  }

  def maximum(t: Tree[Int]): Int = {
    def maximum(t: Tree[Int], curr: Int): Int = {
      t match {
        case Leaf(a) => a.max(curr)
        case Branch(l, r) => maximum(r, maximum(l, curr))
      }
    }
    maximum(t, Integer.MIN_VALUE)
  }

  def depth(t: Tree[_]): Int = sys.error("todo")

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = sys.error("todo")

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = sys.error("todo")

  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[T](t: Tree[T])(implicit ev: Numeric[T]): T = sys.error("todo")

  def depthViaFold[A](t: Tree[A]): Int = sys.error("todo")
}
