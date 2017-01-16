package com.atomist.source

import com.atomist.source.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceUnderPathTest extends FlatSpec with Matchers {

  it should "handle underPath with valid argument" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "content")
    val orig = ValidSource + Seq(f1, f2)

    val withdrawn = orig.underPath("my")
    withdrawn.totalFileCount should equal(orig.totalFileCount - 1)
    val again = orig.underPath("my/new")
    again.totalFileCount should equal(withdrawn.totalFileCount - 1)

    // Now check path rebasing
    withdrawn.directories.map(_.name).toSet should equal(Set("new", "other"))
    again.allFiles.head.parentPathElements.toSet should equal(Set("path"))
  }

  it should "handle underPath matching file not directory should thrown IllegalArgumentException" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "content")
    val f3 = StringFileArtifact("mid.txt", "my/other", "content")
    val orig = ValidSource + Seq(f1, f2, f3)
    an[IllegalArgumentException] should be thrownBy {
      orig / "my/other/mid.txt"
    }
  }

  it should "handle underPath with non-matching argument" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "content")
    val orig = ValidSource + Seq(f1, f2)

    val withdrawn = orig / "randomJunk"
    withdrawn.empty shouldBe true
  }

  it should "handle underPath with Java path" in {
    val defaultJavaPath = "src/main/java"
    val f1 = StringFileArtifact("Foo.java", defaultJavaPath, "contents")
    val f2 = StringFileArtifact("Bar.java", defaultJavaPath, "content")
    val orig = ValidSource + Seq(f1, f2)

    val withdrawn = orig / defaultJavaPath
    withdrawn.allFiles.size should equal(2)
  }

  it should "handle withPathAbove with valid argument" in {
    val f0 = StringFileArtifact("inRoot", "contents")
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "content")
    val orig = ValidSource + Seq(f0, f1, f2)

    val added1 = orig.withPathAbove("xx")
    added1.totalFileCount should equal(orig.totalFileCount)
    val again = "zz/yy" /: orig
    again.totalFileCount should equal(orig.totalFileCount)

    added1.findFile("xx/inRoot") should be(defined)
    added1.directories.size should equal(1)
    added1.findDirectory("xx") should be(defined)
    again.findFile("zz/yy/my/new/path/name") should be(defined)
    added1.directories.map(_.name).toSet should equal(Set("xx"))
    again.allFiles.head.parentPathElements.toSet should equal(Set("zz", "yy"))
  }

  it should "return source with underPath empty" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "content")
    val orig = ValidSource + Seq(f1, f2)
    orig / "" should be theSameInstanceAs orig
  }

  it should "handle underPath when adding an ArtifactSource to another " in {
    val f1 = StringFileArtifact(".atomist/editors/Editor.sj", "{}")
    val f2 = StringFileArtifact(".atomist/editors/Editor2.sj", "{}")
    val as = SimpleFileBasedArtifactSource(f1) + SimpleFileBasedArtifactSource(f2)
    as.allFiles.size should be(2)

    val as2 = as / ".atomist"
    val alldirectories = as2.allDirectories
    alldirectories.size should be(1)
    alldirectories.head.path should be("editors")
    as2.allFiles.size should be(2)
  }

  it should "handle underPath when adding ArtifactSource's together " in {
    val f1 = StringFileArtifact(".atomist/editors/Editor.sj", "{}")
    val f2 = StringFileArtifact(".atomist/editors/Editor2.sj", "{}")
    val f3 = StringFileArtifact(".atomist/editors/Editor3.sj", "{}")
    val f4 = StringFileArtifact(".atomist/editors/Editor4.sj", "{}")
    val as = SimpleFileBasedArtifactSource(f1) +
      SimpleFileBasedArtifactSource(f2) +
      SimpleFileBasedArtifactSource(f3) + f4
    as.allFiles.size should be(4)

    val as2 = as / ".atomist"
    val allDirectories = as2.allDirectories
    allDirectories.size should be(1)
    allDirectories.head.path should be("editors")
    as2.allFiles.size should be(4)
  }
}
