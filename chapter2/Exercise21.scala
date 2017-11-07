package org.mingzhe.fpinscala.chapter2

/**
  * 斐波那契数列尾递归
  */
object Exercise21 {
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, next: Int): Int = {
      if (n == 0) 0
      else if (n == 1) {
        next
      } else {
        loop(n - 1, next, prev + next)
      }
    }
    loop(n, 0, 1)
  }

  def main(args: Array[String]) {
    println(Exercise21.fib(0))
    println(Exercise21.fib(1))
    println(Exercise21.fib(2))
    println(Exercise21.fib(3))
    println(Exercise21.fib(4))
    println(Exercise21.fib(5))
    println(Exercise21.fib(6))
    println(Exercise21.fib(7))
    println(Exercise21.fib(8))
    println(Exercise21.fib(9))
    println(Exercise21.fib(10))
    println(Exercise21.fib(11))
  }
}
