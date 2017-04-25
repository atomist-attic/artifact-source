package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactContainerTest extends FlatSpec with Matchers {

  it should "find files in /" in {
    val src = SimpleFileBasedArtifactSource(
      StringFileArtifact("Foo.java",
        """
          |public class Foo {
          |}
        """.stripMargin)
    )
   src.findFile("/Foo.java") shouldBe defined
  }
}
