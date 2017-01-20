package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceDeltasTest extends FlatSpec with Matchers {

  "deltaFrom" should "find no delta between equal ArtifactSources" in {
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

  "plus" should "include all files when adding" in {
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(StringFileArtifact("pancho", "was a bandit"))
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues"))
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (2)
    sum.findFile("pancho") match {
      case Some(f: FileArtifact) if f.path == "pancho" && f.content == "was a bandit" =>
      case _ => fail("pancho file is not in sum")
    }
    sum.findFile("lefty") match {
      case Some(f: FileArtifact) if f.path == "lefty" && f.content == "he could sing the blues" =>
      case _ => fail("lefty file not in sum")
    }
  }

  it should "combine like directories when adding" in {
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(StringFileArtifact("pancho", "was a bandit"),
        StringFileArtifact("federales/say", "we could have had him any day")
      )
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues"),
        StringFileArtifact("federales/we", "only let him slip away")
      )
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (4)
    sum.allDirectories.size should be (1)
    sum.allDirectories.head.path should be ("federales")
  }

  it should "include all directories when adding" in {
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(StringFileArtifact("pancho", "was a bandit"),
        StringFileArtifact("federales/say", "we could have had him any day")
      )
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues"),
        StringFileArtifact("federales/we", "only let him slip away"),
        StringFileArtifact("out/of", "kindness I suppose")
      )
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (5)
    sum.allDirectories.size should be (2)
    sum.findDirectory("federales") match {
      case Some(d: DirectoryArtifact) if d.path == "federales" =>
      case _ => fail("federales directory not in sum")
    }
    sum.findDirectory("out") match {
      case Some(d: DirectoryArtifact) if d.path == "out" =>
      case _ => fail("out directory not in sum")
    }
  }

  it should "handle nested directories when adding" in {
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(StringFileArtifact("pancho", "was a bandit"),
        StringFileArtifact("federales/say", "we could have had him any day")
      )
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues"),
        StringFileArtifact("federales/we", "only let him slip away"),
        StringFileArtifact("federales/out/of", "kindness I suppose")
      )
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (5)
    sum.allDirectories.size should be (2)
    sum.findDirectory("federales") match {
      case Some(d: DirectoryArtifact) if d.path == "federales" =>
      case _ => fail("federales directory not in sum")
    }
    sum.findDirectory("federales/out") match {
      case Some(d: DirectoryArtifact) if d.path == "federales/out" =>
      case _ => fail("federales/out directory not in sum")
    }
  }

  it should "put artifacts in right to cachedDeltas when adding" in {
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(StringFileArtifact("pancho", "was a bandit"))
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues"))
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (2)
    sum.cachedDeltas.size should be (1)
    sum.cachedDeltas.head.path should be ("lefty")
  }

  it should "add artifacts in right to cachedDeltas when adding" in {
    val pancho = StringFileArtifact("pancho", "was a bandit")
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(pancho), Seq(FileAdditionDelta(pancho))
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues"))
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (2)
    sum.cachedDeltas.size should be (2)
    sum.cachedDeltas.exists(_.path == pancho.path) should be (true)
    sum.cachedDeltas.exists(_.path == "lefty") should be (true)
  }

  it should "ignore cachedDeltas in right when adding" in {
    val pancho = StringFileArtifact("pancho", "was a bandit")
    val left: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("left"),
      Seq(pancho), Seq(FileAdditionDelta(pancho))
    )
    val right: ArtifactSource = new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier("right"),
      Seq(StringFileArtifact("lefty", "he could sing the blues")),
      // nothing should exist in cachedDeltas that does not exist in artifacts
      // but we are just testing here
      Seq(FileAdditionDelta(StringFileArtifact("you", "weren't your mama's only boy")))
    )
    val sum: ArtifactSource = left + right

    sum.totalFileCount should be (2)
    sum.cachedDeltas.size should be (2)
    sum.cachedDeltas.exists(_.path == pancho.path) should be (true)
    sum.cachedDeltas.exists(_.path == "lefty") should be (true)
    sum.cachedDeltas.exists(_.path == "you") should be (false)
  }
}
