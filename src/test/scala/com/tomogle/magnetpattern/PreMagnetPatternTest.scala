package com.tomogle.magnetpattern

import org.scalatest.{FlatSpec, Matchers}

class PreMagnetPatternTest extends FlatSpec with Matchers {

  behavior of "PreMagnetPatternTest"

  it should "concatenate 'xyz' to an input String" in new PreMagnetPattern {
    val result = preMagnetFunction("initial")
    result shouldBe "initialxyz"
  }

  it should "increment and convert an int to a long" in new PreMagnetPattern {
    val result = preMagnetFunction(1)
    result should equal(2L)
  }

  it should "add 100 to a long" in new PreMagnetPattern {
    val result = preMagnetFunction(20L)
    result shouldBe 120L
  }

}
