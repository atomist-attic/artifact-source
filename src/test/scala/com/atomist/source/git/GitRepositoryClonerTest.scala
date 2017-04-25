package com.atomist.source.git

import java.io.File
import java.nio.file.Paths

import com.atomist.source.ArtifactSourceCreationException
import org.scalatest.{FlatSpec, Matchers}

class GitRepositoryClonerTest extends FlatSpec with Matchers {

  it should "clone remote repo to temp directory" in {
    cloneAndVerify()
  }

  it should "clone remote repo to specified directory" in {
    val repoDir = Paths.get(System.getProperty("java.io.tmpdir"), s"tmp_${System.currentTimeMillis}").toFile
    repoDir.deleteOnExit()
    cloneAndVerify(None, None, Some(repoDir))
  }

  it should "clone remote repo with branch specified" in {
    cloneAndVerify(Some("path-into-as"), Some("966b8f992fb27558c06ef9dc44b4dcc6cd7626de"))
  }

  it should "fail to clone repo due to malformed git url" in {
    val grc = GitRepositoryCloner("", Some("foo://github.com"))
    an[ArtifactSourceCreationException] should be thrownBy grc.clone("rug", "atomist")
  }

  private def cloneAndVerify(branch: Option[String] = None, sha: Option[String] = None, dir: Option[File] = None): Unit = {
    val grc = GitRepositoryCloner("")
    val start = System.currentTimeMillis
    val as = grc.clone("rug", "atomist", branch, sha, dir)
    val artifacts = as.artifacts
    // println(s"ArtifactSource creation: ${System.currentTimeMillis - start} ms")
    artifacts.size should be > 0
    grc.cleanUp(as.id)
    as.id.rootFile.exists shouldBe false
  }
}