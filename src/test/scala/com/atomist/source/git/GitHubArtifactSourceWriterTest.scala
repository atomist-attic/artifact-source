package com.atomist.source.git

import java.io.FileInputStream
import java.nio.file.Paths

import com.atomist.source.file.{ClassPathArtifactSource, FileSystemArtifactSourceIdentifier, ZipFileArtifactSourceReader, ZipFileInput}
import com.atomist.source.git.TestConstants.Token
import com.atomist.source.{ArtifactSourceTest, SimpleCloudRepoId}
import com.atomist.util.{BinaryDecider, GitRepositoryCloner}
import org.apache.commons.io.IOUtils

class GitHubArtifactSourceWriterTest extends GitHubMutatorTest(Token) {

  private val gitHubWriter = GitHubArtifactSourceWriter(Token)

  def springBootZipFileId: ZipFileInput = {
    val f = ClassPathArtifactSource.classPathResourceToFile("springboot1.zip")
    ZipFileInput(new FileInputStream(f))
  }

  "GitHubArtifactSourceWriter" should "create repository and copy contents in root directory only" in {
    val tempRepo = newTemporaryRepo()

    val helloWorldProject = ClassPathArtifactSource.toArtifactSource("java-source/HelloWorldService.java")
    val cri = SimpleCloudRepoId(tempRepo.getName, tempRepo.getOwnerName)
    val ghid = GitHubArtifactSourceLocator(cri)
    gitHubWriter.write(helloWorldProject, GitHubSourceUpdateInfo(ghid, getClass.getName))
    Thread sleep 1000
    val read = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)
    ArtifactSourceTest.validateCopy(helloWorldProject, read)
  }

  it should "create repository and write contents in many directories" in {
    val tempRepo = newPopulatedTemporaryRepo()

    val springBootProject = ZipFileArtifactSourceReader fromZipSource springBootZipFileId
    val cri = SimpleCloudRepoId(tempRepo.getName, tempRepo.getOwnerName)
    val ghid = GitHubArtifactSourceLocator(cri)
    gitHubWriter.write(springBootProject, GitHubSourceUpdateInfo(ghid, getClass.getName))
    val read = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)
    ArtifactSourceTest.validateCopyAllowingExtras(springBootProject, read)
  }

  it should "clone a remote repository, push contents to a new repository, and verify contents" in {
    val grc = GitRepositoryCloner(Token)
    val cloned = grc.clone("spring-rest-seed", "atomist-seeds")
    cloned shouldBe defined
    val id = FileSystemArtifactSourceIdentifier(cloned.get)
    val as = FileSystemGitArtifactSource(id)

    val newTempRepo = newPopulatedTemporaryRepo()
    val cri = SimpleCloudRepoId(newTempRepo.getName, newTempRepo.getOwnerName)
    gitHubWriter.write(as, GitHubSourceUpdateInfo(GitHubArtifactSourceLocator(cri), "new project from seed"))

    val clonedSeed = grc.clone(cri.repo, cri.owner)
    clonedSeed shouldBe defined
    val clonedAs = FileSystemGitArtifactSource(FileSystemArtifactSourceIdentifier(clonedSeed.get))
    val f = clonedAs.findFile(".mvn/wrapper/maven-wrapper.jar")
    f shouldBe defined

    val jarFile = f.get
    val content = IOUtils.toByteArray(jarFile.inputStream())
    BinaryDecider.isBinaryContent(content) shouldBe true
    val jid = ZipFileInput(Paths.get(clonedAs.id.rootFile.getPath, jarFile.path).toFile)
    val jarSource = ZipFileArtifactSourceReader.fromZipSource(jid)
    jarSource.findFile("META-INF/MANIFEST.MF") shouldBe defined
  }
}
