package org.mingzhe.fpinscala.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    foldRight(ns, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(h, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => l
      case Cons(h, tail) =>
        if (f(h)) {
          dropWhile(tail, f)
        } else {
          Cons(h, dropWhile(tail, f))
        }
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(h, tail) =>
        if (tail != Nil) {
          Cons(h, init(tail))
        } else {
          Cons(h, Nil)
        }
    }

  def length[A](l: List[A]): Int = ???

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A, B](l: List[A])(f: A => B): List[B] = ???
}