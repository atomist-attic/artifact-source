package com.atomist.util

import com.atomist.source.TestUtils
import org.scalatest.{FlatSpec, Matchers}
import com.atomist.util.IgnoredFilesFinder.ignoredFiles

class IgnoredFilesFinderTest extends FlatSpec with Matchers {

  import TestUtils.TestIgnoreFilesRoot

  it should "ignore files in .atomistignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-atomistignore")
    files.size should be(8)
  }

  it should "find no ignored files in .atomistignore and .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-atomistignore-and-gitignore")
    files shouldBe empty
  }

  it should "ignore file in .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-gitignore-1")
    files.size should be(1)
  }

  it should "find no ignored files in .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/dot-gitignore-2")
    files shouldBe empty
  }

  it should "find no ignored files with no .gitignore file" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/no-dot-gitignore")
    files shouldBe empty
  }

  it should "find ignored files with empty .atomistignore and non-empty .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/empty-dot-atomistignore-non-empty-gitignore")
    files.size should be(2)
  }

  it should "find files with empty .atomistignore in subdirectory" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/empty-dot-atomistignore-and-gitignore")
    files.size should be(2)
  }

  it should "find no files with empty .gitignore" in {
    val files = ignoredFiles(s"$TestIgnoreFilesRoot/empty-dot-gitignore")
    files shouldBe empty
  }
}
