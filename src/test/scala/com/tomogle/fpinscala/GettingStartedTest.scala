package com.tomogle.fpinscala

import org.scalatest.{FlatSpec, Matchers}

class GettingStartedTest extends FlatSpec with Matchers {

  behavior of "fib"
  import GettingStarted.fib

  it should "give 0 for 1st element" in {
    fib(1) shouldBe 0
  }

  it should "give 1 for 2nd element" in {
    fib(2) shouldBe 1
  }

  it should "give 1 for 3rd element" in {
    fib(3) shouldBe 1
  }

  it should "give 2 for 4th element" in {
    fib(4) shouldBe 2
  }

  it should "give 3 for 5th element" in {
    fib(5) shouldBe 3
  }

  it should "give 5 for 6th element" in {
    fib(6) shouldBe 5
  }

  behavior of "isSorted"

  import GettingStarted.isSorted

  it should "give true for a sorted Int array" in {
    isSorted(Array(1,2,3), (a: Int, b: Int) => a < b) shouldBe true
  }

  it should "give false for an Int array with out of order element in middle" in {
    isSorted(Array(1,2,1,3), (a: Int, b: Int) => a < b) shouldBe false
  }

  it should "give false for an Int array with out of order element at start" in {
    isSorted(Array(2,1,3), (a: Int, b: Int) => a < b) shouldBe false
  }

  it should "give false for an Int array with out of order element at end" in {
    isSorted(Array(1,2,3,4,5,6,7,2), (a: Int, b: Int) => a < b) shouldBe false
  }

  behavior of "curry"
  import GettingStarted.curry

  it should "give the same behavior when curried as uncurried" in {
    val f = (a: Int, b: Int) => a * b
    val curriedF: (Int) => (Int) => Int = curry(f)

    val fResult = f(1,3)
    val curriedFResult = curriedF(1)(3)
    fResult should equal(curriedFResult)
  }

  behavior of "unCurry"
  import GettingStarted.unCurry

  it should "give the same behavior when curried as uncurried" in {
    val f = (a: Int) => (b: Int) => a * b
    val unCurriedF = unCurry(f)

    val fResult = f(1)(8)
    val unCurriedFResult = unCurriedF(1, 8)
    fResult should equal(unCurriedFResult)
  }

  behavior of "compose"
  import GettingStarted.compose
  it should "give the same behavior when composed as when called as f(g())" in {
    val f = (b: Int) => b.toString
    val g = (a: Int) => a * 3

    val composeResult = compose(f, g)(2)
    val fgResult = f(g(2))
    composeResult should equal(fgResult)
  }
}
