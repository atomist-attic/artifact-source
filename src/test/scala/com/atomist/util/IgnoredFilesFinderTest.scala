package com.atomist.util

import com.atomist.source.TestUtils
import org.scalatest.{FlatSpec, Matchers}
import com.atomist.util.IgnoredFilesFinder.ignoredFiles

class IgnoredFilesFinderTest extends FlatSpec with Matchers {

  import TestUtils._

  it should "ignore files specified in .atomistignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-atomistignore")
    files.size should be(8)
  }

  it should "find no files with .atomistignore and .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-atomistignore-and-gitignore")
    files shouldBe empty
  }

  it should "find file in .gitignore in 1st test source" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-gitignore-1")
    files.size should be(1)
  }

  it should "find no files with .gitignore in 2nd test source" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-gitignore-2")
    files shouldBe empty
  }

  it should "find no files without .gitignore file" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/no-dot-gitignore")
    files shouldBe empty
  }

  it should "find no files with empty .atomistignore and .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/empty-dot-atomistignore-and-gitignore")
    files.size should be(2)
  }

  it should "find no files with empty .atomistignore in subdirectory" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/empty-dot-atomistignore-and-gitignore-2")
    files.size should be(2)
  }
}
