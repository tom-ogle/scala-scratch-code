package com.tomogle.fizzbuzz

import org.scalatest.{FlatSpec, Matchers}

class ExtensibleFizzBuzzerTest extends FlatSpec with Matchers {
  
//  val fizzBuzzerUnderTest = new BasicExtensibleFizzBuzzer(
//    Seq(divisibleBy3And5, divisibleBy3, divisibleBy5),
//    _.toString
//  )
//
//  behavior of "ExtensibleFizzBuzzer#run"
//
//  it should "return a sequence the same length as the input sequence" in {
//    val range = 1 to 10
//    val result = fizzBuzzerUnderTest.run(range)
//    result should have length 10
//  }
//
//  it should "return FizzBuzz for multiple of 3 and 5" in {
//    val input = Seq(15, 30, 45, 60, 75, 90)
//    val result = fizzBuzzerUnderTest.run(input)
//    result should equal(Seq("FizzBuzz", "FizzBuzz", "FizzBuzz","FizzBuzz", "FizzBuzz", "FizzBuzz"))
//  }
//
//  it should "return Fizz for multiples of 3 (and not multiple of 5)" in {
//    val input = Seq(3, 6, 9, 12)
//    val result = fizzBuzzerUnderTest.run(input)
//    result should equal(Seq("Fizz", "Fizz", "Fizz", "Fizz"))
//  }
//
//  it should "return Buzz for multiples of 5 (and not multiple of 3)" in {
//    val input = Seq(5, 10, 20, 100)
//    val result = fizzBuzzerUnderTest.run(input)
//    result should equal(Seq("Buzz", "Buzz", "Buzz", "Buzz"))
//  }
//
//  it should "return the original number for non-multiples of 3 or 5" in {
//    val input = Seq(1, 2, 4, 7)
//    val result = fizzBuzzerUnderTest.run(input)
//    result should equal(Seq("1", "2", "4", "7"))
//  }
}
