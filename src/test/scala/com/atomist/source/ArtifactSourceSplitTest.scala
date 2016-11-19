package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceSplitTest extends FlatSpec with Matchers {

  "ArtifactSource.split" should "work on empty ArtifactSources" in {
    val a = EmptyArtifactSource("a")
    val b = EmptyArtifactSource("b")
    val split = a.split(SplitCriteria(shared = f => f.name.endsWith(".java"), toA = f => f.name.endsWith(".scala")))
    split._1.empty shouldBe true
    split._2.empty shouldBe true
  }
}
