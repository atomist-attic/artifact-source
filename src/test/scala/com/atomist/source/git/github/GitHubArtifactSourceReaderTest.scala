package com.atomist.source.git.github

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.Files

import com.atomist.source._
import com.atomist.source.file._
import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch
import com.atomist.source.git.TestConstants._
import com.atomist.source.git.github.GitHubArtifactSourceLocator.fromStrings
import resource._

class GitHubArtifactSourceReaderTest extends GitHubMutatorTest(Token) {

  private val zw = ZipFileArtifactSourceWriter
  private val seeds = GitHubArtifactSourceLocator.rootOfMaster("spring-rest-seed", "atomist-seeds")
  private val self = ghs sourceFor DefaultGitHubArtifactSourceLocator(repo = "artifact-source", owner = "atomist")

  "GitHubArtifactSourceReader" should "find a known repository" in {
    val artifacts = self.artifacts
    artifacts.size should be > 1
  }

  it should "read artifact-source repository and write contents to zip" in {
    val as = self
    val fO = as.findFile("src/main/scala/com/atomist/source/artifactSource.scala")
    fO shouldBe defined

    val f = Files.createTempFile("tmp", ".zip").toFile
    f.deleteOnExit()
    for (fos <- managed(new FileOutputStream(f))) {
      val zo = StreamingZipFileOutput("foobar", fos)
      zw.write(as, zo, SimpleSourceUpdateInfo(getClass.getName)) shouldBe true
    }

    val zid = ZipFileInput(new FileInputStream(f))
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    val zipFile = zipSource.findFile("src/test/resources/springboot1.zip")
    zipFile shouldBe defined
    f.delete()
  }

  it should "return appropriate ArtifactSourceIdentifier with commit sha" in {
    self.id match {
      case gid: GitHubArtifactSourceIdentifier =>
        gid.commitSha should not be null
        gid.commitSha.equals("") shouldBe false
      case x => fail(s"Unexpected artifact source id $x")
    }
  }

  it should "find content" in {
    val artifacts = self.artifacts
    val files = artifacts.filter(_.isInstanceOf[FileArtifact])
    artifacts.size should be > 1
    val aFile = files.head.asInstanceOf[FileArtifact]
    aFile.contentLength should be > 0L
    val s = aFile.content
    s should have size aFile.contentLength
  }

  it should "support addition" in {
    val newTempRepo = newPopulatedTemporaryRepo()

    val cri = SimpleCloudRepoId(newTempRepo.getName, newTempRepo.getOwnerName)
    val startAs = ghs sourceFor GitHubArtifactSourceLocator(cri, branch = MasterBranch)
    startAs.findFile(testFiles(1).path) shouldBe empty
    val withAddedFiles = startAs + testFiles

    withAddedFiles should not be theSameInstanceAs(startAs)

    startAs.findFile(testFiles(1).path) should not be defined
    withAddedFiles.findFile(testFiles(1).path) shouldBe defined

    (withAddedFiles Δ startAs).deltas.isEmpty shouldBe false
  }

  it should "read org code from root in master" in {
    val newTempRepo = newPopulatedTemporaryRepo()

    val read = ghs sourceFor fromStrings(newTempRepo.getName, newTempRepo.getOwnerName)
    withClue("known repository must be non-empty: ") {
      read.allFiles.nonEmpty shouldBe true
    }
    withClue("known repository must be non-empty: ") {
      read.empty shouldBe false
    }
  }

  it should "read from branch other than master" in {
    val newTempRepo = newPopulatedTemporaryRepo()

    val branch = "test_branch"
    ghs createBranch(newTempRepo, branch, MasterBranch)
    ghs commitFiles(newTempRepo, branch, "new files", testFiles, Seq.empty)

    val cri = SimpleCloudRepoId(newTempRepo.getName, newTempRepo.getOwnerName)
    val readMaster = ghs sourceFor GitHubArtifactSourceLocator(cri, MasterBranch)
    withClue("known repository master must contain more files") {
      readMaster.allFiles.nonEmpty shouldBe true
    }
    val readBranch = ghs sourceFor GitHubArtifactSourceLocator(cri, branch)
    withClue(s"branch $branch must contain more files than master") {
      readBranch.allFiles.size should be > readMaster.allFiles.size
    }
  }

  it should "not find non-existent branch" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val cri = SimpleCloudRepoId(newTempRepo.getName, newTempRepo.getOwnerName)
    an[ArtifactSourceException] should be thrownBy
      (ghs sourceFor GitHubArtifactSourceLocator(cri, "this-is-complete-nonsense"))
  }

  it should "not find non-existent repository" in {
    an[ArtifactSourceException] should be thrownBy
      (ghs sourceFor fromStrings("sdlfdslksdlfksjdlfkjslkdjf-lib", TestOrg))
  }

  it should "not find repository for syntactically incorrect owner" in {
    an[ArtifactSourceException] should be thrownBy
      (ghs sourceFor fromStrings("sdlfdslksdlfksjdlfkjslkdjf-lib", "not-the-droid(owner)-we-are-looking for"))
  }

  it should "not find repository for non-existent owner" in {
    an[ArtifactSourceException] should be thrownBy
      (ghs sourceFor fromStrings("sdlfdslksdlfksjdlfkjslkdjf-lib", "notthedroidswearelookingfor"))
  }

  it should "parse seed project" in {
    val seedSource = ghs sourceFor seeds
    seedSource.findDirectory("src") shouldBe defined
    seedSource.totalFileCount should be > 5

    val targetDir = seedSource.findDirectory("src")
    withClue(s"expectation failed about $targetDir, source files=${ArtifactSourceUtils.prettyListFiles(seedSource)}") {
      targetDir shouldBe defined
      targetDir.get.directories.size should be >= 2
      Set("main", "test").equals(targetDir.get.directories.map(_.name).toSet)
    }
  }

  it should "find all files in directory given path" in {
    val files = self.allFiles
    withClue(s"should have found at least 30 files, not ${files.size}") {
      files.size should be >= 30
    }
    val fO = self.findFile("pom.xml")
    fO shouldBe defined
    val f1 = self.findFile("src/main/scala/com/atomist/source/artifactSource.scala")
    f1 shouldBe defined
  }

  it should "all directory path elements should contain name" in {
    ArtifactSourceTest.directoryPathElementsShouldExistAndContainName(self)
  }

  it should "should find nested path elements" in {
    val files = self.allFiles
    files.exists(_.pathElements.size > 1) should equal(true)
    files.exists(_.pathElements.toSet equals Set("src", "main", "scala", "com", "atomist", "source", "git")) shouldBe true
  }

  it should "not find non-existent sha" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    an[ArtifactSourceException] should be thrownBy
      (ghs treeFor GitHubShaIdentifier(newTempRepo.getName, newTempRepo.getOwnerName, "strongMenAlsoCry"))
  }

  it should "find existing sha" in { // Slow
    val as = self
    val withSha = as.id.asInstanceOf[GitHubArtifactSourceIdentifier]
    val sourceFromTree = ghs treeFor withSha
    sourceFromTree.totalFileCount shouldEqual as.totalFileCount
  }

  it should "read from large repository" in {
    val cloudRepoId = SimpleCloudRepoId(owner = "spring-projects", repo = "spring-framework")
    val as = ghs sourceFor GitHubArtifactSourceLocator(cloudRepoId)
    as.totalFileCount should be > 1000
  }
}
