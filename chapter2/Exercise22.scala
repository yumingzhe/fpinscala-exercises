package org.mingzhe.fpinscala.chapter2

object Exercise22 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    for (i <- 0 to as.length - 2) {
      if (!ordered(as(i), as(i + 1))) {
        return false
      }
    }
    true
  }

  def main(args: Array[String]) {
    val array1 = Array(1, 2, 3, 4, 5, 6)
    println(isSorted(array1, (x: Int, y: Int) => x <= y)) // true
    val array2 = Array(1, 3, 2, 4, 6, 5)
    println(isSorted(array2, (x: Int, y: Int) => x <= y)) // false
  }
}
