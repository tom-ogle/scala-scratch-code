package com.tomogle.magnetpattern

import org.scalatest.{FlatSpec, Matchers}

class MagnetPatternTest extends FlatSpec with Matchers {

  behavior of "MagnetPatternTest"

  it should "concatenate 'xyz' to an input String" in new MagnetPattern {
    val result = magnetFunction("initial")
    result shouldBe "initialxyz"
  }

  it should "increment and convert an int to a long" in new MagnetPattern {
    val result = magnetFunction(1)
    result should equal(2L)
  }

  it should "add 100 to a long" in new MagnetPattern {
    val result = magnetFunction(20L)
    result shouldBe 120L
  }

}
