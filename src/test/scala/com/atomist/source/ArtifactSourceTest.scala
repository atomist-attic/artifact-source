package com.atomist.source

import org.scalatest.Matchers

/**
  * Test for ArtifactSource operations
  */
class ArtifactSourceTest extends CommonTests {

  import TestUtils._

  override val validSource = ValidSource

  override val emptySource = EmptyArtifactSource()

  it should "find valid directory" in {
    val introspectorSource = SpringBootSource
    introspectorSource.findDirectory("src") shouldBe defined
    introspectorSource.findDirectory("junk").isDefined shouldBe false
  }

  it should "known directory path element should contain name" in {
    val introspectorSource = SpringBootSource
    val dirName = "src/main"
    introspectorSource.findDirectory(dirName) match {
      case None => fail("Should have been able to find source directory")
      case Some(d) =>
        d.pathElements.size should equal(2)
        d.pathElements.mkString("/") should endWith(dirName)
    }
  }

  it should "all directory path elements should contain name" in {
    TestUtils.directoryPathElementsShouldExistAndContainName(SpringBootSource)
  }

  it should "not find valid directory in root prepended with /" in {
    val introspectorSource = SpringBootSource
    introspectorSource.findDirectory("/src").isDefined shouldBe false
  }

  it should "find valid nested directory with / convention" in {
    val introspectorSource = SpringBootSource
    introspectorSource.findDirectoryByPath(Seq("src", "main")) shouldBe defined
    introspectorSource.findDirectory("src/main") shouldBe defined
    introspectorSource.findDirectory("other/junk") shouldBe empty
    introspectorSource.findDirectoryByPath(Seq("other", "junk")).isDefined shouldBe false
  }

  it should "find directory in directory" in {
    val introspectorSource = SpringBootSource
    val srcMain = introspectorSource.findDirectoryByPath(Seq("src", "main"))
    srcMain shouldBe defined
    srcMain.get.findChildDirectory("java") shouldBe defined
    srcMain.get.findDirectory("java") shouldBe defined
    srcMain.get.findDirectory("java/com/example") shouldBe defined
    srcMain.get.findFile("java/com/example/WebTemplateApplication.java") shouldBe defined
  }

  it should "not modify source when adding file to root when using java.util.List in implementing ArtifactSource" in {
    class InternallyMutableArtifactSource
      extends ArtifactSource
        with DirectoryInferringArtifactContainer {

      val id = null

      /**
        * All files.
        *
        * @return all file artifacts, ignoring directory structure,
        *         which will still be available from each FileArtifact.
        */
      override val allFiles: Seq[FileArtifact] = Nil
    }

    val f = StringFileArtifact("name", "", "contents")
    val internallyMutableAs = new InternallyMutableArtifactSource()
    val result = internallyMutableAs + Seq(f)
    result.allFiles.size should equal(internallyMutableAs.allFiles.size + 1)
    result.allDirectories.size should equal(internallyMutableAs.allDirectories.size)
    internallyMutableAs.findFile(f.path) should not be defined
  }

  it should "create file artifacts from path/content pairs" in {
    val fileArtifacts = ArtifactSource.of(Map("name1" -> "whatever1", "name2" -> "whatever2", "name3" -> "whatever3"))
    fileArtifacts.size should equal(3)
  }

  it should "create artifact source from file artifacts" in {
    val f1 = StringFileArtifact("name1", "whatever1")
    val f2 = StringFileArtifact("name2", "whatever2")
    val f3 = StringFileArtifact("name3", "whatever3")
    val as = ArtifactSource.fromFiles(f1, f2, f3)
    as.allFiles.size should equal(3)
    as.allFilesAsJava.size should equal(3)
  }

  it should "create artifact source using +" in {
    val f1 = StringFileArtifact(".atomist/editors/name1", "whatever1")
    val f2 = StringFileArtifact(".atomist/editors/name2", "whatever2")
    val f3 = StringFileArtifact(".atomist/editors/name3", "whatever3")
    val as = ArtifactSource.fromFiles(f1, f2) + f3
    as.allFiles.size should equal(3)
    as.allFilesAsJava.size should equal(3)
  }

  it should "create artifact source by adding files, deleting a file and adding it back" in {
    val f1 = StringFileArtifact(".atomist/editors/name1", "whatever1")
    val f2 = StringFileArtifact(".atomist/editors/name2", "whatever2")
    val f3 = StringFileArtifact(".atomist/editors/name3", "whatever3")
    val as = (ArtifactSource.fromFiles(f1, f2) + f3) - f3 + f3
    as.allFiles.size should equal(3)
    as.allFilesAsJava.size should equal(3)
  }
}

object ArtifactSourceTest extends Matchers {

  def validateSourceTree(source: ArtifactSource) {
    val dirName = "src/main"
    source.findDirectory(dirName) match {
      case None => fail("Should have been able to find source directory")
      case Some(d) =>
        d.pathElements.size should equal(2)
        d.pathElements.mkString("/") should equal(dirName)
    }
  }

  /**
    * Validate a copy of an ArtifactSource
    */
  def validateCopy(from: ArtifactSource, to: ArtifactSource): Unit = {
    withClue(s"copy must have same number of files (${to.allFiles.size}") {
      from.allFiles.size should equal(to.allFiles.size)
    }
    verify(from, to)
  }

  /**
    * Same as validateCopy, but allow extra files in the copy
    */
  def validateCopyAllowingExtras(from: ArtifactSource, to: ArtifactSource): Unit = {
    withClue(s"copy must have at least same number of files (${to.allFiles.size}") {
      to.allFiles.size should be >= from.allFiles.size
    }
    verify(from, to)
  }

  def directoryPathElementsShouldExistAndContainName(source: ArtifactSource) {
    source.directories.size should be > 0
    source.directories.foreach(dir => {
      dir.pathElements should not be empty
      dir.pathElements.last should equal(dir.name)
    })
  }

  def verify(expected: ArtifactSource, actual: ArtifactSource): Unit = {
    expected.allFiles.foreach(f => {
      val newFile = actual.findFile(f.path)
      withClue(s"Looking for expected file $f, old artifactSource has ${expected.totalFileCount} files, new artifactSource has ${actual.totalFileCount} files, " +
        s"list=\n${actual.allFiles.map(_.path).mkString("\n")}") {
        newFile shouldBe defined
        newFile.get.name shouldBe f.name
        newFile.get.pathElements shouldBe f.pathElements
        newFile.get.content shouldBe f.content
      }
    })
  }
}


