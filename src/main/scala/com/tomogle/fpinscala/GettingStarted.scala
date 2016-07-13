package com.tomogle.fpinscala

object GettingStarted {
  def fib(n: Int): Int = {

    def loop(n: Int, prev1: Int, prev2: Int): Int = {
      if (n == 0) prev1
      else loop(n - 1, prev2, prev1 + prev2)
    }

    loop(n-1,0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val endIndex = as.length - 1

    def loop(firstIndex: Int, secondIndex: Int, currentResult: Boolean): Boolean = {
      if (!currentResult) currentResult // short circuit the loop
      else if (secondIndex > endIndex) currentResult
      else loop(firstIndex + 1, secondIndex + 1, ordered(as(firstIndex), as(secondIndex)))
    }
    loop(0, 1, currentResult = true)
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  def unCurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def apply[T](elements: T*): MyList[T] =
      if (elements.isEmpty) MyNil
      else MyCons(elements.head, apply(elements.tail: _*))

  }
}
