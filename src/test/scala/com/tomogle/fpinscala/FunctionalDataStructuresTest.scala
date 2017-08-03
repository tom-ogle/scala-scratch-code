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

  it should "Drop based on the predicate" in {
    val result = dropWhile(List(1, 2, 3, 4, 5))(_ <= 3)
    result shouldBe List(4, 5)
  }

  behavior of "append"

  import FunctionalDataStructures.append

  it should "append a List to a List" in {
    val result = append(List(1, 2, 3, 4), List(5, 6))
    result shouldBe List(1, 2, 3, 4, 5, 6)
  }

  it should "append a List to the empty List" in {
    val result = append(List(1, 2, 3, 4), Nil)
    result shouldBe List(1, 2, 3, 4)
  }

  it should "append a Nil to a List" in {
    val result = append(Nil, List(5, 6))
    result shouldBe List(5, 6)
  }

  it should "append a Nil to Nil" in {
    val result = append(Nil, Nil)
    result shouldBe Nil
  }

  behavior of "append2"

  import FunctionalDataStructures.append2

  it should "append a List to a List" in {
    val result = append2(List(1, 2, 3, 4), List(5, 6))
    result shouldBe List(1, 2, 3, 4, 5, 6)
  }

  it should "append a List to the empty List" in {
    val result = append2(List(1, 2, 3, 4), Nil)
    result shouldBe List(1, 2, 3, 4)
  }

  it should "append a Nil to a List" in {
    val result = append2(Nil, List(5, 6))
    result shouldBe List(5, 6)
  }

  it should "append a Nil to Nil" in {
    val result = append2(Nil, Nil)
    result shouldBe Nil
  }

  behavior of "concat"

  import FunctionalDataStructures.concat

  it should "append a List of Lists" in {
    val result = concat(List(
      List(1, 2, 3),
      List(4),
      List(5, 6)
    ))
    result shouldBe List(1, 2, 3, 4, 5, 6)
  }

  it should "append a List of Lists including Nil" in {
    val result = concat(List(
      Nil,
      List(1, 2, 3),
      Nil,
      List(4),
      List(5, 6)
    ))
    result shouldBe List(1, 2, 3, 4, 5, 6)
  }

  behavior of "concat2"

  import FunctionalDataStructures.concat2

  it should "append a List of Lists" in {
    val result = concat2(List(
      List(1, 2, 3),
      List(4),
      List(5, 6)
    ))
    result shouldBe List(1, 2, 3, 4, 5, 6)
  }

  it should "append a List of Lists including Nil" in {
    val result = concat2(List(
      Nil,
      List(1, 2, 3),
      Nil,
      List(4),
      List(5, 6)
    ))
    result shouldBe List(1, 2, 3, 4, 5, 6)
  }

  behavior of "plusOne"

  import FunctionalDataStructures.plusOne

  it should "add one to the elements of a List" in {
    val result = plusOne(List(1, 2, 3, 4, 5))
    result shouldBe List(2, 3, 4, 5, 6)
  }

  behavior of "doubleToString"

  import FunctionalDataStructures.doubleToString

  it should "convert Double to String" in {
    val result = doubleToString(List(1.1, 2.2, 3.3, 4.4, 5.5))
    result shouldBe List("1.1", "2.2", "3.3", "4.4", "5.5")
  }

  behavior of "map"

  import FunctionalDataStructures.map

  it should "apply a function to every element of a List" in {
    val result = map(List(1.1, 2.2, 3.3, 4.4, 5.5))(_.toString)
    result shouldBe List("1.1", "2.2", "3.3", "4.4", "5.5")
  }

  behavior of "filter"

  import FunctionalDataStructures.filter

  it should "apply a function to filter the elements of a List" in {
    val result = filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)
    result shouldBe List(2, 4, 6)
  }

  behavior of "flatMap"

  import FunctionalDataStructures.flatMap

  it should "apply a function to every element of a List" in {
    val result = flatMap(List(1, 2, 3, 4, 5, 6))(i => List(i, i))
    result shouldBe List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
  }

  behavior of "filter2"

  import FunctionalDataStructures.filter2

  it should "apply a function to filter the elements of a List" in {
    val result = filter2(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)
    result shouldBe List(2, 4, 6)
  }

  behavior of "sumCorrespondingElements"

  import FunctionalDataStructures.sumCorrespondingElements

  it should "sum corresponding elements in two Lists" in {
    val result = sumCorrespondingElements(List(1, 2, 3, 4, 5, 6), List(6, 5, 4, 3, 2, 1))
    result shouldBe List(7, 7, 7, 7, 7, 7)
  }

  behavior of "zipWith"

  import FunctionalDataStructures.zipWith

  it should "zip corresponding elements in two Lists" in {
    val result = zipWith(List(1, 2, 3, 4, 5, 6), List("6", "5", "4", "3", "2", "1"))(_ + _)
    result shouldBe List("16", "25", "34", "43", "52", "61")
  }

  behavior of "hasSubsequence"

  import FunctionalDataStructures.hasSubsequence

  it should "Be true when there is a subsequence at the start" in {
    val result = hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(1))
    result shouldBe true
  }

  it should "Be true when there is a subsequence in the middle" in {
    val result = hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(3, 4, 5))
    result shouldBe true
  }

  it should "Be true when there is a subsequence at the end" in {
    val result = hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(6, 7))
    result shouldBe true
  }

  it should "Be false when there is not a subsequence" in {
    val result = hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(3, 4, 6))
    result shouldBe false
  }

  it should "Be true when they are both the empty List" in {
    val result = hasSubsequence(Nil, Nil)
    result shouldBe true
  }

  it should "Be true when the subsequence is the empty List" in {
    val result = hasSubsequence(List(1, 2, 3), Nil)
    result shouldBe true
  }

  it should "Be false when the full sequence is the empty List and the subsequence has elements" in {
    val result = hasSubsequence(Nil, List(1, 2))
    result shouldBe false
  }

}
