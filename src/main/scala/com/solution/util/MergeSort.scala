package com.solution.util

object MergeSort {

  def apply[A](xs: List[A])(implicit ordering: Ordering[A]): List[A] = {
    mergeSort(xs)
  }

  private def mergeSort[A](xs: List[A])(implicit ordering: Ordering[A]): List[A] = {
    val n = xs.length / 2
    if (n == 0) {
      xs
    } else {
      def merge(xs: List[A], ys: List[A]): List[A] = {
        (xs, ys) match {
          case(Nil, ys) => ys
          case(xs, Nil) => xs
          case(x :: xs1, y :: ys1) =>
            if (ordering.compare(x, y) == -1) {
              x :: merge(xs1, ys)
            } else {
              y :: merge(xs, ys1)
            }
        }
      }
      val (left, right) = xs.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }
}
