package com.atomist.source.git

import org.scalatest.{FlatSpec, Matchers}

class GitRepositoryClonerTest extends FlatSpec with Matchers {

  it should "clone remote repo" in {
    cloneAndVerify("", "")
  }

  it should "shallow clone remote repo with branch specified" in {
    cloneAndVerify("path-into-as", "966b8f992fb27558c06ef9dc44b4dcc6cd7626de")
  }

  private def cloneAndVerify(branch: String, sha: String): Unit = {
    val grc = new GitRepositoryCloner("", "https://github.com")
    val start = System.currentTimeMillis
    val as = grc.clone("rug", "atomist", branch, sha)
    val artifacts = as.artifacts
    println(s"ArtifactSource creation: ${System.currentTimeMillis - start} ms")
    artifacts.size should be > 0
    grc.cleanUp(as.id)
    as.id.rootFile.exists shouldBe false
  }
}