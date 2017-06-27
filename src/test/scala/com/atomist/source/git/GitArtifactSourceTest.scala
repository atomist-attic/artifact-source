package com.atomist.source.git

import java.nio.file.Paths

import com.atomist.source.file.FileSystemArtifactSourceIdentifier
import org.scalatest.{FlatSpec, Matchers}

class GitArtifactSourceTest extends FlatSpec with Matchers {

  "GitArtifactSourceTest" should "create GitArtifactSource from Git repo on disk" in {
    val id = FileSystemArtifactSourceIdentifier(Paths.get(System.getProperty("user.dir")).toFile)
    val as = GitArtifactSource(id)
    as.findFile("pom.xml") shouldBe defined
    as.findDirectory(".git") shouldBe empty
  }

  it should "fail to create GitArtifactSource in a directory without a .git folder" in {
    val id = FileSystemArtifactSourceIdentifier(Paths.get(System.getProperty("java.io.tmpdir")).toFile)
    an[IllegalArgumentException] should be thrownBy GitArtifactSource(id)
  }
}
