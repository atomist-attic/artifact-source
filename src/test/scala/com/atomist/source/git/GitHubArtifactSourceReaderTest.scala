package com.atomist.source.git

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.Files

import com.atomist.source._
import com.atomist.source.file._
import com.atomist.source.git.GitHubArtifactSourceLocator.MasterBranch
import com.atomist.source.git.TestConstants._
import com.atomist.util.Utils._
import org.scalatest._

class GitHubArtifactSourceReaderTest extends FlatSpec with Matchers {

  private val gitHubReader = GitHubServices(TestConstants.Token)
  private val zw = ZipFileArtifactSourceWriter
  private val defaultTemplates = GitHubArtifactSourceLocator.rootOfMaster("project-templates", "atomisthq")
  private val self = gitHubReader.sourceFor(DefaultGitHubArtifactSourceLocator(repo = "artifact-source", owner = "atomist"))

  "GitHub" should "find a known repository" in {
    val artifacts = self.artifacts
    artifacts.size should be > 1
  }

  it should "read artifact-source repository and write contents to zip" in {
    val as = self
    val fO = as.findFile("src/main/scala/com/atomist/source/artifactSource.scala")
    fO shouldBe defined

    val f = Files.createTempFile("tmp", ".zip").toFile
    f.deleteOnExit()
    withCloseable(new FileOutputStream(f))(fos => {
      val zo = StreamingZipFileOutput("foobar", fos)
      zw.write(as, zo, SimpleSourceUpdateInfo(getClass.getName)) shouldBe true
    })

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
    val files = artifacts.filter(a => a.isInstanceOf[FileArtifact])
    artifacts.size should be > 1
    val aFile = files.head.asInstanceOf[FileArtifact]
    aFile.contentLength should be > 0L
    val s = aFile.content
    s should have size aFile.contentLength
  }

  it should "support addition" in {
    val files: Seq[Artifact] = Seq(
      StringFileArtifact("somethingOrOther.txt", "The quick brown fox jumped over the lazy dog"),
      StringFileArtifact("scripts2/other.txt", "This file isn't in the root"),
      StringFileArtifact("another2/directory/tree/extra.txt", "Nested file")
    )
    val startAs = gitHubReader sourceFor GitHubArtifactSourceLocator(TestConstants.TestTargetRepoInfo, branch = MasterBranch)
    startAs.findFile(files(1).path) should not be defined
    val withAddedFiles = startAs + files

    withAddedFiles should not be theSameInstanceAs(startAs)

    startAs.findFile(files(1).path) should not be defined
    withAddedFiles.findFile(files(1).path) shouldBe defined

    (withAddedFiles Δ startAs).deltas.isEmpty shouldBe false
  }

  it should "read org code from root in master" in {
    val read = gitHubReader.sourceFor(GitHubArtifactSourceLocator.fromStrings(TestTargetRepo, TestOrg))
    withClue("known repository must be non-empty: ") {
      read.allFiles.nonEmpty shouldBe true
    }
    withClue("known repository must be non-empty: ") {
      read.empty shouldBe false
    }
  }

  it should "read from branch other than master" in {
    val readMaster = gitHubReader.sourceFor(GitHubArtifactSourceLocator(TestTargetRepoInfo, MasterBranch))
    withClue("known repository master must contain more files") {
      readMaster.allFiles.nonEmpty shouldBe true
    }
    val readBranch = gitHubReader.sourceFor(GitHubArtifactSourceLocator(TestTargetRepoInfo, TestTargetExpectedBranch))
    withClue(s"branch $TestTargetExpectedBranch must contain more files than master") {
      readBranch.allFiles.size should be > readMaster.allFiles.size
    }
  }

  it should "not find non-existent branch" in {
    an[ArtifactSourceException] should be thrownBy
      (gitHubReader sourceFor GitHubArtifactSourceLocator(TestTargetRepoInfo, "this-is-complete-nonsense"))
  }

  it should "not find non-existent repository" in {
    an[ArtifactSourceException] should be thrownBy
      (gitHubReader sourceFor GitHubArtifactSourceLocator.fromStrings("sdlfdslksdlfksjdlfkjslkdjf-lib", TestOrg))
  }

  it should "not find repository for syntactically incorrect owner" in {
    an[ArtifactSourceException] should be thrownBy
      (gitHubReader sourceFor GitHubArtifactSourceLocator.fromStrings("sdlfdslksdlfksjdlfkjslkdjf-lib", "not-the-droid(owner)-we-are-looking for"))
  }

  it should "not find repository for non-existent owner" in {
    an[ArtifactSourceException] should be thrownBy
      (gitHubReader sourceFor GitHubArtifactSourceLocator.fromStrings("sdlfdslksdlfksjdlfkjslkdjf-lib", "notthedroidswearelookingfor"))
  }

  it should "parse templates project" in {
    val projectTemplateSource = gitHubReader sourceFor defaultTemplates
    projectTemplateSource.findDirectory("spring-boot") shouldBe defined
    projectTemplateSource.totalFileCount should be > 5

    val targetDir = projectTemplateSource.findDirectory("spring-boot/rest-service")
    withClue(s"expectation failed about $targetDir, source files=${ArtifactSourceUtils.prettyListFiles(projectTemplateSource)}") {
      targetDir shouldBe defined
      targetDir.get.directories.size should be >= 2
      Set("project", "meta").equals(targetDir.get.directories.map(_.name).toSet)
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
    self.allFiles.exists(_.pathElements.size > 1) should equal(true)
    self.allFiles.exists(_.pathElements.toSet equals Set("src", "main", "scala", "com", "atomist", "source", "git")) shouldBe true
  }

  it should "not find non-existent sha" in {
    an[ArtifactSourceException] should be thrownBy
      (gitHubReader treeFor GitHubShaIdentifier(TestTargetRepo, TestOrg, "strongMenAlsoCry"))
  }

  it should "find non-existent sha" in {
    val as = self
    val withSha = as.id.asInstanceOf[GitHubArtifactSourceIdentifier]
    val sourceFromTree = gitHubReader treeFor withSha
    ArtifactSourceTest.validateCopy(as, sourceFromTree)
  }

  it should "read from large repository" in {
    val cloudRepoId = SimpleCloudRepoId(owner = "spring-projects", repo = "spring-framework")
    val as = gitHubReader sourceFor GitHubArtifactSourceLocator(cloudRepoId)
    as.totalFileCount should be > 1000
  }
}
