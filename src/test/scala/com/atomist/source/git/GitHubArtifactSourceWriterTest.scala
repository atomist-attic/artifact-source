package com.atomist.source.git

import java.io.FileInputStream

import com.atomist.source.file.{ClassPathArtifactSource, ZipFileArtifactSourceReader, ZipFileInput}
import com.atomist.source.git.TestConstants.Token
import com.atomist.source.{ArtifactSourceTest, SimpleCloudRepoId}

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
}
