package com.atomist.source.git

import java.io.File
import java.nio.file.Paths

import com.atomist.source.ArtifactSourceCreationException
import com.atomist.source.file.FileSystemArtifactSource
import org.scalatest.{FlatSpec, Matchers}

class GitRepositoryClonerTest extends FlatSpec with Matchers {

  "GitRepositoryCloner" should "clone remote repo to temp directory" in {
    cloneAndVerify()
  }

  it should "clone remote repo to specified directory" in {
    val repoDir = createRepoDir
    cloneAndVerify(None, None, Some(repoDir))
  }

  it should "clone remote repo to specified directory, reset directory content, and clone again" in {
    val repoDir = createRepoDir

    val grc = GitRepositoryCloner("")
    val as = grc.clone("rug", "atomist", None, None, Some(repoDir))
    as.allArtifacts.size should be > 0
    grc.resetDirectoryContent(repoDir)
    val newAs = FileSystemArtifactSource(as.id)
    newAs.artifacts.size should equal(0)
    val recloned = grc.clone("rug", "atomist", None, None, Some(repoDir))
    recloned.artifacts.size should be > 0
    grc.cleanUp(repoDir)
  }

  it should "clone remote repo with branch specified" in {
    cloneAndVerify(Some("path-into-as"), Some("966b8f992fb27558c06ef9dc44b4dcc6cd7626de"))
  }

  ignore should "clone repo and reset to specified sha and branch without knowing depth" in {
    val repoDir = createRepoDir

    val grc = GitRepositoryCloner("")
    // val start = System.currentTimeMillis
    val as = grc.clone("spring-service-demo", "atomisthqa", None, Some("6617df4994b0916a633bd12dfa6ce71ce8814976"), Some(repoDir))
    as.allArtifacts.size should be > 0
    // println(s"ArtifactSource creation: ${System.currentTimeMillis - start} ms")
  }

  it should "fail to clone repo due to malformed git url" in {
    val grc = GitRepositoryCloner("", Some("foo://github.com"))
    an[ArtifactSourceCreationException] should be thrownBy grc.clone("rug", "atomist")
  }

  private def createRepoDir = {
    val repoDir = Paths.get(System.getProperty("java.io.tmpdir"), s"tmp_${System.currentTimeMillis}").toFile
    repoDir.deleteOnExit()
    repoDir
  }

  private def cloneAndVerify(branch: Option[String] = None, sha: Option[String] = None, dir: Option[File] = None): Unit = {
    val grc = GitRepositoryCloner("")
    // val start = System.currentTimeMillis
    val as = grc.clone("rug", "atomist", branch, sha, dir)
    val artifacts = as.allArtifacts
    // println(s"ArtifactSource creation: ${System.currentTimeMillis - start} ms")
    artifacts.size should be > 0
    val fid = as.id
    fid.name should equal("rug")
    grc.cleanUp(fid)
    fid.rootFile.exists shouldBe false
  }
}