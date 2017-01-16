package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests to run against different representations of ArtifactSource:
  * Directory browsing and File based.
  */
abstract class CommonTests extends FlatSpec with Matchers {

  /**
    * Must have same contents as TestUtils.ValidSource.
    */
  protected val validSource: ArtifactSource

  /**
    * Create an empty ArtifactSource.
    */
  protected val emptySource: ArtifactSource

  it should "report non-empty for valid source" in {
    validSource.empty shouldBe false
  }

  it should "find file in root by path" in {
    val as = emptySource
    val f1 = StringFileArtifact("name", Nil, "some content")
    as.findFile("name") should not be defined
    val as2 = as plus f1
    as2.findFileByPath("name", Nil) shouldBe defined
  }

  it should "find file by deeper path" in {
    val as = emptySource
    val pathElements = Seq("some", "path")
    val f1 = StringFileArtifact("name", pathElements, "some content")
    as.findFile("name") should not be defined
    val as2 = as plus f1
    as2.findFile("name") should not be defined
    as2.findFile(s"${pathElements.mkString("/")}/name") shouldBe defined
    as2.findFile(s"${pathElements.mkString("/")}/name").get should equal(f1)
  }

  it should "find file in root" in {
    val as = emptySource
    val f1 = StringFileArtifact("name", Nil, "some content")
    as.findFile("name") should not be defined
    val as2 = as plus f1
    as2.findFile("name") shouldBe defined
    as2.findFile("name").get should equal(f1)
  }

  it should "have same artifacts as itself cached" in {
    val cached = validSource.cached
    TestUtils.verify(validSource, cached)
  }

  it should "have all resources cached when cached" in {
    val cached = validSource.cached
    cached.allFiles.forall(_.isCached) shouldBe true
  }

  it should "add extra file" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val result = validSource + f1
    result.allFiles.size should equal(validSource.allFiles.size + 1)
    val f2 = StringFileArtifact("name2", "my/other/path", "contents")
    val result2 = result + f2
    result2.allFiles.size should equal(validSource.allFiles.size + 2)
  }

  it should "add extra file via Seq" in {
    val f = StringFileArtifact("name", "my/new/path", "contents")
    val result = validSource + Seq(f)
    result.allFiles.size should equal(validSource.allFiles.size + 1)
  }

  it should "add extra file under existing path" in {
    val alreadyThere = StringFileArtifact("src/test.txt", "already there")
    val src1 = StringFileArtifact("src/thing", "under src")
    val src2 = StringFileArtifact("src/main/otherThing", "under src/main")

    val left = SimpleFileBasedArtifactSource(alreadyThere)
    val right = SimpleFileBasedArtifactSource(src1, src2)

    val result = left + right
    result.allFiles.size should equal(left.totalFileCount + right.totalFileCount)
  }

  it should "add no extra directory when adding to root" in {
    val f = StringFileArtifact("name", "", "contents")
    val result = validSource + Seq(f)
    result.allFiles.size should equal(validSource.allFiles.size + 1)
    result.allDirectories.size should equal(validSource.allDirectories.size)
  }

  it should "not modify source when adding file to root" in {
    val f = StringFileArtifact("name", "", "contents")
    val result = validSource + Seq(f)
    result.allFiles.size should equal(validSource.allFiles.size + 1)
    result.allDirectories.size should equal(validSource.allDirectories.size)
    validSource.findFile(f.path) should not be defined
  }

  it should "add empty directory to root" in {
    val dirName = "dirxxx"
    val d = EmptyDirectoryArtifact(dirName)
    val result = validSource + d
    result.allFiles.size should equal(validSource.allFiles.size)
    result.allDirectories.size should equal(validSource.allDirectories.size + 1)
  }

  it should "add two extra directories with non-conflicting merge when adding multiple files" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name2", "my/other", "contents")
    val result = validSource + Seq(f1, f2)
    validSource.findDirectory("my") should not be defined
    result.findDirectory("my") shouldBe defined
    result.findDirectoryByPath(Seq("my", "new")) shouldBe defined
    result.findDirectory("my/new/path") shouldBe defined
    result.findDirectory("my/other") shouldBe defined

    result.allFiles.size should equal(validSource.allFiles.size + 2)
    // Allow for new directories implied by file: /my, /my/new, /my/new/path, /my/other
    result.allDirectories.size should equal(validSource.allDirectories.size + 4)
  }

  it should "add empty directory to new subfolder" in {
    val as = emptySource
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "contents")
    val as2 = as plus Seq(f1, f2)

    val dirName = "dirxxx"
    val d = EmptyDirectoryArtifact(dirName, Seq("newFolder"))
    val result = as2 + d
    result.allFiles.size should equal(as2.allFiles.size)
    result.allDirectories.map(_.path).sorted should equal(
      Seq("my", "my/new", "my/new/path", "my/other", "my/other/path", "newFolder", "newFolder/dirxxx").sorted)
    result.allDirectories.size should equal(as2.allDirectories.size + 2)
    result.findDirectory(s"newFolder/$dirName") shouldBe defined
  }

  it should "add empty directory to existing subfolder" in {
    val as = emptySource
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val f2 = StringFileArtifact("name", "my/other/path", "contents")
    val as2 = as plus Seq(f1, f2)

    val dirName = "dirxxx"
    val d = EmptyDirectoryArtifact(dirName, Seq("my"))
    val result = as2 + d
    result.allFiles.size should equal(as2.allFiles.size)
    result.allDirectories.size should equal(as2.allDirectories.size + 1)
    result.findDirectory(s"my/$dirName") shouldBe defined
    result.findDirectory("my").get.artifacts.size should be(3)
  }

  it should "add extra directories when adding single file" in {
    val f1 = StringFileArtifact("name", "my/new/path", "contents")
    val result = validSource + Seq(f1)
    validSource.findDirectory("my") should not be defined
    result.findDirectory("my") shouldBe defined
    result.findDirectoryByPath(Seq("my", "new")) shouldBe defined
    result.findDirectoryByPath(Seq("my", "new", "path")) shouldBe defined
    result.findDirectory("my/new/path") shouldBe defined

    result.allFiles.size should equal(validSource.allFiles.size + 1)
    // Allow for new directories implied by file: /my, /my/new, /my/new/path, /my/other
    result.allDirectories.size should equal(validSource.allDirectories.size + 3)
  }
}
