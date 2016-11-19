package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceDeltasTest extends FlatSpec with Matchers {

  it should "find no delta between equal ArtifactSources" in {
    val as = EmptyArtifactSource()
    val f1 = StringFileArtifact("name", "", "contents")
    val withAdd1 = as + f1
    val withAdd2 = as + f1
    val deltas = withAdd2 Δ withAdd1
    deltas.empty shouldBe true
  }

  it should "find no delta between identical ArtifactSources" in {
    val as = EmptyArtifactSource()
    val f1 = StringFileArtifact("name", "", "contents")
    val withAdd = as + f1
    val deltas = withAdd Δ withAdd
    deltas.empty shouldBe true
  }

  it should "process delta of addition in root directory from empty" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", "", "contents")
    val withAdd = as + f1
    val deltas = withAdd Δ as
    deltas.deltas.size should equal(1)
    deltas.deltas.head.path should equal(f1.path)
  }

  it should "process delta of deletion in root directory" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", "", "contents")
    val withFile = as + f1
    val withoutFile = withFile filter(d => true, f => false)
    withoutFile.empty shouldBe true
    val deltas = withoutFile Δ withFile
    deltas.deltas.size should equal(1)
    deltas.deltas.head.path should equal(f1.path)
    deltas.deltas.head.isInstanceOf[FileDeletionDelta] shouldBe true
  }

  it should "process delta of modification in root directory" in {
    val originalContent = "whatever"
    val as = EmptyArtifactSource()
    val f1 = StringFileArtifact("name", "", originalContent)
    val start = as + f1
    val edited = start ✎ SimpleFileEditor(file => true, f => StringFileArtifact(f.path, f.content.reverse))

    val deltas = edited Δ start
    deltas.deltas.size should equal(1)
    deltas.deltas.iterator.next match {
      case fu: FileUpdateDelta =>
        fu.path should equal(f1.path)
        fu.oldFile.content should equal(originalContent)
        fu.updatedFile.content should equal(originalContent.reverse)
      case _ =>
    }
  }

  it should "handle delta via add files" in {
    var startAs: ArtifactSource = EmptyArtifactSource()
    startAs = startAs + StringFileArtifact("some1.txt", "The quick brown fox jumped over the lazy dog")

    val files: Seq[Artifact] = Seq(
      StringFileArtifact("some.txt", "The quick brown fox jumped over the lazy dog"),
      StringFileArtifact("scripts/other.txt", "This file isn't in the root"),
      StringFileArtifact("another/directory/tree/extra.txt", "Nested file")
    )
    val withAddedFiles = startAs + files

    (withAddedFiles Δ startAs).empty shouldBe false
  }

  it should "note added empty directories in deltas" is pending
}
