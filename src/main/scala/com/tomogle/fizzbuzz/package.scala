package com.tomogle

package object fizzbuzz {

  trait FizzBuzzer[A,B] {
    def run(range: Seq[A]): Seq[B] = range map assignValue
    def assignValue(element: A): B
  }

  object BasicFizzBuzzer extends FizzBuzzer[Int, String] {

    def assignValue(element: Int): String = {
      val divisibleBy3 = element % 3 == 0
      val divisibleBy5 = element % 5 == 0

      if (divisibleBy3 && divisibleBy5) "FizzBuzz"
      else if (divisibleBy3) "Fizz"
      else if (divisibleBy5) "Buzz"
      else element.toString
    }
  }

  object Mappings {
    val divisibleBy3And5: (Test, String) = (element => element % 3 == 0 && element % 5 == 0, "FizzBuzz")
    val divisibleBy3: (Test, String) = (_ % 3 == 0, "Fizz")
    val divisibleBy5: (Test, String) = (_ % 5 == 0, "Buzz")
  }

  type Mapping = (Test, String)
  type Test = Int => Boolean

  class ExtensibleFizzBuzzer(mappings: Seq[Mapping]) extends FizzBuzzer[Int, String] {

    def assignValue(element: Int): String = {
      val firstMatchingMapping = mappings.find { mapping =>
        val (test, result) = mapping
        test(element)
      }
      firstMatchingMapping map {
        case (_, result) => result
      } getOrElse element.toString
    }
  }
}
