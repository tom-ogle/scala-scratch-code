package com.tomogle.fizzbuzz


object FizzBuzz {
  def main(args: Array[String]) {
    FizzBuzzer.run(1 to 100) map println
  }

}
object FizzBuzzer {
  def run(range: Seq[Int]): Seq[String] = range map assignValue

  private def assignValue(element: Int): String = {
    val divisibleBy3 = element % 3 == 0
    val divisibleBy5 = element % 5 == 0

    if (divisibleBy3 && divisibleBy5) "FizzBuzz"
    else if (divisibleBy3) "Fizz"
    else if (divisibleBy5) "Buzz"
    else element.toString
  }
}
