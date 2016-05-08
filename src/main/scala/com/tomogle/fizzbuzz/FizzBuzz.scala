package com.tomogle.fizzbuzz

import com.tomogle.fizzbuzz.Mappings._

object FizzBuzz {

  def main(args: Array[String]) {
    val fizzBuzzer = new ExtensibleFizzBuzzer(Seq(divisibleBy3And5, divisibleBy3, divisibleBy5))
    fizzBuzzer.run(1 to 100) foreach println
  }
}

