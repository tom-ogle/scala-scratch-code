package com.tomogle.fpinscala

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FunctionalDataStructuresTest extends FlatSpec with Matchers {

  behavior of "foldRight"

  import FunctionalDataStructures.foldRight

  it should "foldRight" in {
    val result = foldRight(List("1", "2", "3"), "0")(_ + _)
    result shouldBe "1230"
  }

  it should "" in {
    val r = List("1", "2", "3").foldRight("0")(_ + _)
  }

  behavior of "foldRight2"

  import FunctionalDataStructures.foldRight2

  it should "foldRight2" in {
    val result = foldRight2(List("1", "2", "3"), "0")(_ + _)
    result shouldBe "1230"
  }

  behavior of "foldLeft"

  import FunctionalDataStructures.foldLeft

  it should "foldLeft" in {
    val result = foldLeft(List("1", "2", "3"), "0")(_ + _)
    result shouldBe "0123"
  }

  behavior of "foldLeft2"

  import FunctionalDataStructures.foldLeft2

  it should "foldLeft2" in {
    val result = foldLeft2(List("1", "2", "3"), "0")(_ + _)
    result shouldBe "0123"
  }

  behavior of "reverse"

  import FunctionalDataStructures.reverse

  it should "reverse a List" in {
    val result = reverse(List(1, 2, 3))
    result shouldBe List(3, 2, 1)
  }

  it should "return a 1 element List unchanged" in {
    val result = reverse(List(1))
    result shouldBe List(1)
  }

  it should "return Nil when applied to Nil" in {
    val result = reverse(Nil)
    result shouldBe Nil
  }

  behavior of "reverse2"

  import FunctionalDataStructures.reverse2

  it should "reverse a List" in {
    val result = reverse2(List(1, 2, 3))
    result shouldBe List(3, 2, 1)
  }

  it should "return a 1 element List unchanged" in {
    val result = reverse2(List(1))
    result shouldBe List(1)
  }

  it should "return Nil when applied to Nil" in {
    val result = reverse2(Nil)
    result shouldBe Nil
  }

  behavior of "reverse3"

  import FunctionalDataStructures.reverse3

  it should "reverse a List" in {
    val result = reverse3(List(1, 2, 3, 4, 5, 6))
    result shouldBe List(6, 5, 4, 3, 2, 1)
  }

  it should "return a 1 element List unchanged" in {
    val result = reverse3(List(1))
    result shouldBe List(1)
  }

  it should "return Nil when applied to Nil" in {
    val result = reverse3(Nil)
    result shouldBe Nil
  }

  behavior of "length"

  it should "give the length of a List" in {
    val result = FunctionalDataStructures.length(List(1, 2, 3))
    result shouldBe 3
  }

  it should "give the length of an empty List" in {
    val result = FunctionalDataStructures.length(Nil)
    result shouldBe 0
  }

  behavior of "length2"

  it should "give the length2 of a List" in {
    val result = FunctionalDataStructures.length2(List(1, 2, 3))
    result shouldBe 3
  }

  it should "give the length of an empty List" in {
    val result = FunctionalDataStructures.length2(Nil)
    result shouldBe 0
  }

  behavior of "sum"

  it should "give the sum of a List of Ints" in {
    val result = FunctionalDataStructures.sum(List(1, 2, 3, 4))
    result shouldBe 10
  }

  it should "give zero for a Nil List" in {
    val result = FunctionalDataStructures.sum(Nil)
    result shouldBe 0
  }

  behavior of "product"

  it should "give the product of a List of Ints" in {
    val result = FunctionalDataStructures.product(List(1, 2, 3, 4))
    result shouldBe 24
  }

  it should "give 1 for a Nil List" in {
    val result = FunctionalDataStructures.product(Nil)
    result shouldBe 1
  }

  behavior of "init"

  import FunctionalDataStructures.init

  it should "Return all but the last element of a List" in {
    val result = init(List(1, 2, 3, 4, 5))
    result shouldBe List(1, 2, 3, 4)
  }

  it should "Return Nil when applied to Nil" in {
    val result = init(Nil)
    result shouldBe Nil
  }

  behavior of "tail"

  import FunctionalDataStructures.tail

  it should "Return the tail of a List" in {
    val result = tail(List(1, 2, 3, 4))
    result shouldBe List(2, 3, 4)
  }

  it should "Return Nil for a 1 element List" in {
    val result = tail(List(1))
    result shouldBe Nil
  }

  it should "Return Nil for a Nil List" in {
    val result = tail(Nil)
    result shouldBe Nil
  }

  behavior of "setHead"

  import FunctionalDataStructures.setHead

  it should "Replace the head of a List " in {
    val result = setHead(List(2, 3, 4), 1)
    result shouldBe List(1, 3, 4)
  }

  it should "Set the head of Nil " in {
    val result = setHead(Nil, 1)
    result shouldBe List(1)
  }

  behavior of "drop"

  import FunctionalDataStructures.drop

  it should "Drop the first n elements" in {
    val result = drop(List(1, 2, 3, 4, 5), 3)
    result shouldBe List(4, 5)
  }

  it should "Return Nil when dropping from Nil" in {
    val result = drop(Nil, 3)
    result shouldBe Nil
  }

  behavior of "dropWhile"

  import FunctionalDataStructures.dropWhile

  it should "Drop based on the pridcate" in {
    val result = dropWhile(List(1, 2, 3, 4, 5))(_ <= 3)
    result shouldBe List(4, 5)
  }
}
