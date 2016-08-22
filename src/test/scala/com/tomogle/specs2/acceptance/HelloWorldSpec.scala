package com.tomogle.specs2.acceptance

import org.specs2.Specification

class HelloWorldSpec extends Specification { def is =
  s2"""
      Specification to check the 'Hello, world' String

      The 'Hello, world' String should
        contain 12 characters             $e1
        start with 'Hello'                $e2
        end with 'world'                  $e3
    """

  val underTest = "Hello, world"

  def e1 = underTest must have size 12
  def e2 = underTest must startWith("Hello")
  def e3 = underTest must endWith("world")

}
