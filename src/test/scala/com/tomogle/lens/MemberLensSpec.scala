package com.tomogle.lens

import org.scalatest.{FlatSpec, Matchers}

class MemberLensSpec extends FlatSpec with Matchers {

  behavior of "member Lens get"

  it should "get the value for a key that is present in the Map" in {
    val memberLens = member[Int, String](1)
    val result = memberLens get Map(1 -> "value")
    result shouldBe Some("value")
  }

  it should "get a None for a key that is not present in the Map" in {
    val memberLens = member[Int, String](1)
    val result = memberLens get Map(2 -> "value")
    result shouldBe None
  }

  behavior of "member Lens set"

  it should "return the map with an extra key for a given value" in {
    val memberLens = member[Int, String](2)
    val result = memberLens.set(Map(1 -> "value"), Some("New value"))
    result shouldBe Map(1 -> "value", 2 -> "New value")
  }

  it should "return the map replacing a given value" in {
    val memberLens = member[Int, String](1)
    val result = memberLens.set(Map(1 -> "value"), Some("New value"))
    result shouldBe Map(1 -> "New value")
  }

  it should "return the map unmodified for no value" in {
    val memberLens = member[Int, String](1)
    val result = memberLens.set(Map(1 -> "value"), None)
    result shouldBe Map(1 -> "value")
  }

  behavior of "member Lens mod"

  it should "modify the value only for the specified key" in {
    val memberLens = member[Int, String](1)
    val result = memberLens.mod ({
      case Some("value") => Option("new value")
      case s@Some(_) => s
      case None => None
    }, Map(1 -> "value", 2 -> "value"))
    result shouldBe Map(1 -> "new value", 2 -> "value")
  }

}
