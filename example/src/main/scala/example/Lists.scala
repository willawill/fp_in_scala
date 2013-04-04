package example

import common._

object Lists {
  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    def subSum(current: Int, remain: List[Int]) : Int = {
      if (remain.isEmpty) current
      else subSum(current + remain.head, remain.tail)
    }
    if (xs.isEmpty) throw new NoSuchElementException
    else subSum(xs.head, xs.tail)
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    def subMax(currentMax: Int, remain: List[Int]) : Int = {
      if (remain.isEmpty) currentMax
      else {
        if (remain.head > currentMax) subMax(remain.head, remain.tail)
        else subMax(currentMax, remain.tail)
      }
    }
    if (xs.isEmpty) throw new NoSuchElementException
    else subMax(xs.head, xs.tail)
  }

  def factorial(n: Int) : Int = {
    def sub_fac(acc: Int, n: Int) : Int = {
      if (n == 0) acc
      else sub_fac(acc * n, n - 1)
    }
    sub_fac(1, n)
  }
}
