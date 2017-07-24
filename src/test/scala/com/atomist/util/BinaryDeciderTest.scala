package com.atomist.util

import org.apache.commons.io.IOUtils
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{DiagrammedAssertions, FunSpec, OneInstancePerTest}
import resource.managed

class BinaryDeciderTest
  extends FunSpec
    with DiagrammedAssertions
    with TableDrivenPropertyChecks
    with OneInstancePerTest {

  val data = Table(
    ("content", "isBinary"),
    ("Mary had a little lamb.".getBytes, false),
    ("".getBytes, false),
    (getContent("/spring-boot/web-template/.mvn/wrapper/maven-wrapper.properties"), false),
    (getContent("/spring-boot/web-template/.mvn/wrapper/maven-wrapper.jar"), true),
    (getContent("/spring-boot/web-template/src/main/resources/atomist-logo-horiz.png"), true)
  )

  describe("BinaryDeterminator") {
    it("should determine if binary content") {
      forAll(data) { (content, isBinary) =>
        assert(BinaryDecider.isBinaryContent(content) == isBinary)
      }
    }
  }

  private def getContent(path: String): Array[Byte] =
    managed(getClass.getResourceAsStream(path)).acquireAndGet(IOUtils.toByteArray)
}
