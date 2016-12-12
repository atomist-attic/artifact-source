package com.atomist.util

import com.atomist.util.Octal.{intToOctal, octalToInt}
import org.scalatest.{FlatSpec, Matchers}

class OctalTest extends FlatSpec with Matchers {

  it should "empty string should error" in {
    intercept[IllegalArgumentException] {
      octalToInt("")
    }
  }

  it should "invalid octal should error" in {
    intercept[IllegalArgumentException] {
      octalToInt("1239")
    }

    intercept[IllegalArgumentException] {
      octalToInt("FF")
    }
  }

  it should "handle zeros" in {
    octalToInt("00000000") should equal(0)
  }

  it should "handle 100755" in {
    octalToInt("100755") should equal(33261)
  }

  it should "handle 100644" in {
    octalToInt("100644") should equal(33188)
  }

  it should "handle Int 0" in {
    intToOctal(0) should equal("0")
  }

  it should "handle Int to multi-digit octal" in {
    intToOctal(9) should equal("11")
    intToOctal(342391) should equal("1234567")
  }

  it should "handle Int to octal with trailing zeros" in {
    intToOctal(8) should equal("10")
  }
}
