package com.atomist.source.git

import java.io.File
import java.nio.file.Paths

import com.atomist.source.git.TestConstants.Token
import com.atomist.source.git.github.{GitHubArtifactSourceLocator, GitHubMutatorTest, GitHubSourceUpdateInfo}
import com.atomist.source.{SimpleCloudRepoId, StringFileArtifact}
import com.atomist.util.BinaryDecider
import org.apache.commons.io.FileUtils

class GitRepositoryClonerTest extends GitHubMutatorTest(Token) {

  "GitRepositoryCloner" should "clone remote repo to temp directory" in {
    cloneAndVerify()
  }

  it should "clone remote repo to specified directory" in {
    val repoDir = createRepoDir
    cloneAndVerify(None, None, Some(repoDir))
  }

  it should "clone remote repo to specified directory and clone again to same directory" in {
    val repoDir = createRepoDir
    cloneAndVerify(None, None, Some(repoDir))
    cloneAndVerify(None, None, Some(repoDir))
  }

  it should "clone remote repo to specified directory, reset directory content, and clone again" in {
    val grc = GitRepositoryCloner()
    val cloned = grc.clone("rug", "atomist") match {
      case Left(e) => fail(e)
      case Right(repoDir) => repoDir
    }
    val size = FileUtils.sizeOf(cloned)
    size should be > 0L
    grc.cleanRepoDirectory(cloned)
    FileUtils.sizeOf(cloned) shouldEqual 0L
    val recloned = grc.clone("rug", "atomist", None, None, Some(cloned)) match {
      case Left(e) => fail(e)
      case Right(repoDir) => repoDir
    }
    FileUtils.sizeOf(recloned) shouldEqual size
    grc.deleteRepoDirectory(cloned)
  }

  it should "clone remote repo with branch specified" in {
    cloneAndVerify(Some("path-into-as"), Some("966b8f992fb27558c06ef9dc44b4dcc6cd7626de"))
  }

  it should "clone repo and reset to specified sha and branch without knowing depth" in {
    val repoDir = createRepoDir
    val grc = GitRepositoryCloner()
    // val start = System.currentTimeMillis
    val cloned = grc.clone("artifact-source", "atomist", None, Some("4983a4822e885ee3e1d917d9b1d980bedef349c1"), Some(repoDir)) match {
      case Left(e) => fail(e)
      case Right(dir) => dir
    }
    FileUtils.sizeOf(cloned) should be > 0L
    // println(s"Cloning: ${System.currentTimeMillis - start} ms")
  }

  it should "clone new repo and reset to specified sha and branch without knowing depth" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName
    val cri = SimpleCloudRepoId(repo, owner)
    val files = testFiles :+ StringFileArtifact("somethingOrOther.txt", testFileContents) // Duplicate

    val newBranchSource = GitHubArtifactSourceLocator(cri, "master")
    val multiFileCommitMessage = s"multi file commit at ${System.currentTimeMillis}"
    val fileCommit = ghs commitFiles(GitHubSourceUpdateInfo(newBranchSource, multiFileCommitMessage), files, Seq.empty)
    fileCommit.isEmpty shouldBe false

    val commits = ghs getCommits(repo, owner)
    val sha = commits.last.sha
    val repoDir = createRepoDir
    val grc = GitRepositoryCloner(Token)
    // val start = System.currentTimeMillis
    val cloned = grc.clone(repo, owner, None, Some(sha), Some(repoDir), 1) match {
      case Left(e) => fail(e)
      case Right(dir) => dir
    }
    FileUtils.sizeOf(cloned) should be > 0L
    // println(s"Cloning: ${System.currentTimeMillis - start} ms")
  }

  it should "clone remote repo and verify file contents" in {
    val grc = GitRepositoryCloner()
    val cloned = grc.clone("artifact-source", "atomist") match {
      case Left(e) => fail(e)
      case Right(repoDir) => repoDir
    }
    val f = Paths.get(cloned.getPath, "src", "test", "resources", "springboot1.zip").toFile
    val content = FileUtils.readFileToByteArray(f)
    BinaryDecider.isBinaryContent(content) shouldBe true
  }

  it should "fail to clone repo due to malformed git url" in {
    val grc = GitRepositoryCloner("", Some("foo://github.com"))
    grc.clone("rug", "atomist")
  }

  private def createRepoDir = {
    val repoDir = Paths.get(System.getProperty("java.io.tmpdir"), s"tmp_${System.currentTimeMillis}").toFile
    repoDir.deleteOnExit()
    repoDir
  }

  private def cloneAndVerify(branch: Option[String] = None, sha: Option[String] = None, dir: Option[File] = None): Unit = {
    val grc = GitRepositoryCloner()
    val start = System.currentTimeMillis
    val cloned = grc.clone("rug", "atomist", branch, sha, dir) match {
      case Left(e) => fail(e)
      case Right(repoDir) => repoDir
    }
    val size = FileUtils.sizeOf(cloned)
    size should be > 0L
    println(s"Cloning: ${System.currentTimeMillis - start} ms")
    grc.deleteRepoDirectory(cloned)
  }
}