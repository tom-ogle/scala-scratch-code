package com.tomogle.specs2.unit

import org.specs2.mutable.Specification

class BasicSpec extends Specification {

  "this is a test specification" >> {
    "where 1st example must be true" >> {
      1 must_== 1
    }
    "where 2nd example must be true" >> {
      2 must_== 2
    }
  }

}
