package com.atomist.source.git

import java.io.File
import java.nio.file.Paths

import org.scalatest.{FlatSpec, Matchers}

class GitRepositoryClonerTest extends FlatSpec with Matchers {

  it should "clone remote repo to temp directory" in {
    cloneAndVerify("", "")
  }

  it should "clone remote repo to specified directory" in {
    val repoDir = Paths.get(System.getProperty("java.io.tmpdir"), s"tmp_${System.currentTimeMillis}").toFile
    cloneAndVerify("", "", Some(repoDir))
  }

  it should "shallow clone remote repo with branch specified" in {
    cloneAndVerify("path-into-as", "966b8f992fb27558c06ef9dc44b4dcc6cd7626de")
  }

  private def cloneAndVerify(branch: String, sha: String, dir: Option[File] = None): Unit = {
    val grc = new GitRepositoryCloner("", Some("https://github.com"))
    val start = System.currentTimeMillis
    val as = grc.clone("rug", "atomist", branch, sha, dir)
    val artifacts = as.artifacts
    println(s"ArtifactSource creation: ${System.currentTimeMillis - start} ms")
    artifacts.size should be > 0
    grc.cleanUp(as.id)
    as.id.rootFile.exists shouldBe false
  }
}