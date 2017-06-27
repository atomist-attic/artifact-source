package com.atomist.util

import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import org.scalatest.{FlatSpec, Matchers}

class GitRepositoryClonerTest extends FlatSpec with Matchers {

  "GitRepositoryClonerTest" should "clone remote repo to temp directory" in {
    cloneAndVerify()
  }

  it should "clone remote repo to specified directory" in {
    val repoDir = createRepoDir
    cloneAndVerify(None, None, Some(repoDir))
  }

  it should "clone remote repo to specified directory, reset directory content, and clone again" in {
    val grc = GitRepositoryCloner()
    val cloned = grc.clone("rug", "atomist")
    cloned shouldBe defined
    val repoDir = cloned.get
    val size = FileUtils.sizeOf(repoDir)
    size should be > 0L
    grc.resetDirectoryContent(repoDir)
    FileUtils.sizeOf(repoDir) shouldEqual 0L
    val recloned = grc.clone("rug", "atomist", None, None, cloned)
    recloned shouldBe defined
    FileUtils.sizeOf(recloned.get) shouldEqual size
    grc.cleanUp(repoDir)
  }

  it should "clone remote repo with branch specified" in {
    cloneAndVerify(Some("path-into-as"), Some("966b8f992fb27558c06ef9dc44b4dcc6cd7626de"))
  }

  ignore should "clone repo and reset to specified sha and branch without knowing depth" in {
    val repoDir = createRepoDir
    val grc = GitRepositoryCloner()
    // val start = System.currentTimeMillis
    val cloned = grc.clone("test1", "spring-team", None, Some("f94a3593c7f87df0f3e39667299e76c2420b0cb5"), Some(repoDir))
    cloned shouldBe defined
    FileUtils.sizeOf(cloned.get) should be > 0L
    // println(s"Cloning: ${System.currentTimeMillis - start} ms")
  }

  it should "fail to clone repo due to malformed git url" in {
    val grc = GitRepositoryCloner("", "foo://github.com")
    grc.clone("rug", "atomist") shouldBe empty
  }

  private def createRepoDir = {
    val repoDir = Paths.get(System.getProperty("java.io.tmpdir"), s"tmp_${System.currentTimeMillis}").toFile
    repoDir.deleteOnExit()
    repoDir
  }

  private def cloneAndVerify(branch: Option[String] = None, sha: Option[String] = None, dir: Option[File] = None): Unit = {
    val grc = GitRepositoryCloner()
    // val start = System.currentTimeMillis
    val cloned = grc.clone("rug", "atomist", branch, sha, dir)
    val repoDir = cloned.get
    val size = FileUtils.sizeOf(repoDir)
    size should be > 0L
    // println(s"Cloning: ${System.currentTimeMillis - start} ms")
    grc.cleanUp(repoDir)
  }
}