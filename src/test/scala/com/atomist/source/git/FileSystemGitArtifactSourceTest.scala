package com.atomist.source.git

import java.nio.file.Paths

import com.atomist.source.file.NamedFileSystemArtifactSourceIdentifier
import org.scalatest.{FlatSpec, Matchers}

class FileSystemGitArtifactSourceTest extends FlatSpec with Matchers {

  "FileSystemGitArtifactSourceTest" should "create a FileSystemGitArtifactSource from Git repo on disk" in {
    val id = NamedFileSystemArtifactSourceIdentifier("artifact-source", Paths.get(System.getProperty("user.dir")).toFile)
    val as = FileSystemGitArtifactSource(id)
    as.findFile("pom.xml") shouldBe defined
    as.findDirectory(".git") shouldBe empty
  }

  it should "fail to create a FileSystemGitArtifactSource in a directory without a .git folder" in {
    val id =  NamedFileSystemArtifactSourceIdentifier("artifact-source", Paths.get(System.getProperty("java.io.tmpdir")).toFile)
    an[IllegalArgumentException] should be thrownBy FileSystemGitArtifactSource(id)
  }
}
