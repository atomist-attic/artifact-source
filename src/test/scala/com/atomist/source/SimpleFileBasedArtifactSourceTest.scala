package com.atomist.source

import com.atomist.source.ArtifactSourceUtils.prettyListFiles

class SimpleFileBasedArtifactSourceTest extends CommonTests {

  override val validSource: ArtifactSource = new SimpleFileBasedArtifactSource("",
    StringFileArtifact("HelloWorldService.java", "public class HelloWorldService {}")
  )

  override val emptySource = new SimpleFileBasedArtifactSource("", Nil)

  it should "honor empty" in {
    val fbac = new SimpleFileBasedArtifactSource("", Nil)
    fbac.empty shouldBe true
  }

  it should "add file in root" in {
    val f1 = StringFileArtifact("name", "some content")
    val fbac = new SimpleFileBasedArtifactSource("", f1)
    fbac.allFiles.size should equal(1)
    fbac.artifacts.size should equal(1)
    fbac.allDirectories.size should equal(0)
    fbac.findFile(f1.name) shouldBe defined
    fbac.findFile(f1.name).get.content should equal(f1.content)
  }

  it should "add file in nested directory" in {
    val pathElements = Seq("some", "path")
    val f1 = StringFileArtifact("name", pathElements, "some content")
    val fbac = new SimpleFileBasedArtifactSource("", f1)
    fbac.findFile("name") should not be defined
    fbac.findFile(s"${pathElements.mkString("/")}/name") shouldBe defined
    fbac.findFile(s"${pathElements.mkString("/")}/name").get should equal(f1)
    fbac.allDirectories.map(_.path) should contain allOf("some", "some/path")
    fbac.allDirectories.size should equal(2)

    fbac.directories.isEmpty shouldBe false
  }

  it should "correctly retrieve files from directory" in {
    val pathElements = Seq("some", "path")
    val f1 = StringFileArtifact("name", pathElements, "some content")
    val fbac = new SimpleFileBasedArtifactSource("", f1)
    val targetDir = fbac.findDirectory("some/path")
    targetDir shouldBe defined
    withClue(s"expectation about ${targetDir.get}, source files=${prettyListFiles(fbac)}") {
      targetDir.get.files.size should equal(1)
      targetDir.get.directories.size should equal(0)
      targetDir.get.files.head.name should equal(f1.name)
    }

    fbac.directories.isEmpty shouldBe false
  }

  it should "correctly retrieve all files from nested directories" in {
    val f1 = StringFileArtifact("some/path/name", "some content")
    val f2 = StringFileArtifact("some/path/name2", "some content")
    val f3 = StringFileArtifact("some/path/deeper/name2", "some content")
    val f4 = StringFileArtifact("some/path/deeper/and/deeper/name2", "some content")
    val f5 = StringFileArtifact("in-root", "some content")
    val f6 = StringFileArtifact("other/whatever", "some content")
    val f7 = StringFileArtifact("other/deeper/something", "some content")

    val fbac = new SimpleFileBasedArtifactSource("", Seq(f1, f2, f3, f4, f5, f6, f7))
    fbac.files.size should equal(1)
    fbac.allFiles.size should equal(7)
    fbac.directories.size should equal(2)

    val otherPathDir = fbac.findDirectory("other")
    otherPathDir.get.files.size should equal(1)
    otherPathDir.get.allDirectories.size should equal(1)
    otherPathDir.get.artifacts.size should equal(2)
    otherPathDir.get.allFiles.size should equal(2)

    val somePathDir = fbac.findDirectory("some/path")
    somePathDir shouldBe defined
    withClue(s"expectation about ${somePathDir.get}, source files=${prettyListFiles(fbac)}") {
      somePathDir.get.files.size should equal(2)
      somePathDir.get.allFiles.size should equal(4)
      somePathDir.get.directories.size should equal(1)
      somePathDir.get.files.head.name should equal(f1.name)
    }

    fbac.directories.isEmpty shouldBe false
  }

  it should "correctly retrieve directories from directory" in {
    val pathElements = Seq("some", "path")
    val f1 = StringFileArtifact("name", pathElements, "some content")
    val fbac = new SimpleFileBasedArtifactSource("", f1)
    val targetDir = fbac.findDirectory("some")
    targetDir shouldBe defined
    withClue(s"expectation about ${targetDir.get}, source files=${prettyListFiles(fbac)}") {
      targetDir.get.files.size should equal(0)
      targetDir.get.directories.size should equal(1)
      targetDir.get.directories.head.name should equal("path")
    }

    fbac.directories.isEmpty shouldBe false
  }

  it should "have a way of adding or recognizing empty directories" is pending
}
