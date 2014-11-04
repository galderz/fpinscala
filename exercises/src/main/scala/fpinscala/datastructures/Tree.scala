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

  def depth(t: Tree[_]): Int = {
    def depth(t: Tree[_], acc: Int): Int = {
      t match {
        case Leaf(a) => acc
        case Branch(l, r) =>
          depth(l, acc + 1) max depth(r, acc + 1)
      }
    }
    depth(t, 0)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[T](t: Tree[T])(implicit ev: Numeric[T]): T =
    fold(t)(a => a)(ev.max)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)(1 + _.max(_))
}
