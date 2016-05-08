package com.tomogle.fizzbuzz

object FizzBuzz {

  def main(args: Array[String]) {
    val fizzBuzzer = new BasicExtensibleFizzBuzzer(
      Seq(Mappings.divisibleBy3And5, Mappings.divisibleBy3, Mappings.divisibleBy5),
      _.toString
    )
    fizzBuzzer.run(1 to 100) foreach println
  }

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

  type Mapping[A,B] = (Test[A], B)
  type Test[A] = A => Boolean

  object Mappings {
    val divisibleBy3And5: Mapping[Int, String] = (element => element % 3 == 0 && element % 5 == 0, "FizzBuzz")
    val divisibleBy3: Mapping[Int, String] = (_ % 3 == 0, "Fizz")
    val divisibleBy5: Mapping[Int, String] = (_ % 5 == 0, "Buzz")
  }

  trait ExtensibleFizzBuzzer[A, B] extends FizzBuzzer[A, B] {
    val mappings: Seq[Mapping[A, B]]
    val baseElementTransform: A => B

    def assignValue(element: A): B = {
      val firstMatchingMapping = mappings.find {
        case (test: Test[A], _)  => test(element)
      }
      firstMatchingMapping map {
        case (_, result) => result
      } getOrElse baseElementTransform(element)
    }
  }

  class BasicExtensibleFizzBuzzer(val mappings: Seq[Mapping[Int, String]],
                                  val baseElementTransform: Int => String) extends ExtensibleFizzBuzzer[Int, String]
}

