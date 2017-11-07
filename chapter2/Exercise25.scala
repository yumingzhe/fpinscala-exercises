package org.mingzhe.fpinscala.chapter2

object Exercise25 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
