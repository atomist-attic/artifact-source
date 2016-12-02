package com.atomist.source

import java.io.File

import com.atomist.source.file.ClassPathArtifactSource
import org.scalatest.Matchers

/**
  * Utilities for ArtifactSourceTests
  */
object TestUtils extends Matchers {

  val ValidSource = ClassPathArtifactSource.toArtifactSource("java-source/HelloWorldService.java")

  val SpringBootSource = ClassPathArtifactSource.toArtifactSource("spring-boot/web-template")

  def validateSourceTree(source: ArtifactSource) {
    val dirName = "src/main"
    source.findDirectory(dirName) match {
      case None => fail("Should have been able to find source directory")
      case Some(d) =>
        d.pathElements.size should equal(2)
        d.pathElements.mkString(File.separator) should equal(dirName)
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
      newFile shouldBe defined
      val file = newFile.get
      file.name shouldBe f.name
      file.pathElements shouldBe f.pathElements
      file.content shouldBe f.content
      file.mode shouldBe f.mode
    })
  }
}
