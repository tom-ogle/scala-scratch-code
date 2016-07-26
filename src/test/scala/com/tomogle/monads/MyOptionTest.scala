package com.tomogle.monads

import org.scalatest.{FlatSpec, Matchers}

class MyOptionTest extends FlatSpec with Matchers {

  behavior of "MyOption flatMap"

  it should "return a MyNone when used on a MyNone" in {
    val none: MyOption[Int] = MyNone
    val result = none.flatMap(x => MyOption(x.toString))
    result should equal(MyNone)
  }

  it should "apply the function when used on a MySome" in {
    val some: MyOption[Int] = MyOption(1)
    val result = some.flatMap(x => MyOption((x + 1).toString))
    result should equal(MyOption("2"))
  }

}
