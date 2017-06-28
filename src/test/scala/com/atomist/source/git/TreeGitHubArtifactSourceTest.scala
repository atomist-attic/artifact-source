package com.atomist.source.git

import java.io.FileInputStream

import com.atomist.source._
import com.atomist.source.file.{ClassPathArtifactSource, ZipFileArtifactSourceReader, ZipFileInput}
import com.atomist.source.filter.ArtifactFilter
import com.atomist.source.git.TestConstants.{Token, _}

class TreeGitHubArtifactSourceTest
  extends GitHubMutatorTest(Token) {

  private val githubWriter = GitHubArtifactSourceWriter(Token)

  def springBootZipFileId: ZipFileInput = {
    val f = ClassPathArtifactSource.classPathResourceToFile("springboot1.zip")
    ZipFileInput(new FileInputStream(f))
  }

  "file retrieval" should "work" in {
    val cri = SimpleCloudRepoId(TestTargetRepo, TestOrg)
    val tghas = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)
    val files = tghas.allFiles
    files.size should be > 1
    files.foreach(_.path should not be null)
  }

  "file retrieval and filter" should "work" in {
    val cri = SimpleCloudRepoId(TestTargetRepo, TestOrg)
    val tghas = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs, new MarkdownFilter)
    val files = tghas.allFiles
    files.size should be > 0
    files.foreach(_.path should not be null)
  }

  "file retrieval for larger repository" should "work" in {
    val cri = SimpleCloudRepoId("spring-framework", "spring-projects")
    val tghas = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)
    tghas.allFiles.foreach(_.path should not be null)
    tghas.totalFileCount should be > 1000
  }

  "file retrieval and edit" should "work" in {
    val tempRepo = newPopulatedTemporaryRepo()
    val cri = SimpleCloudRepoId(tempRepo.getName, tempRepo.getOwnerName)

    val springBootProject = ZipFileArtifactSourceReader fromZipSource springBootZipFileId
    val ghid = GitHubArtifactSourceLocator(cri)
    githubWriter.write(springBootProject, GitHubSourceUpdateInfo(ghid, getClass.getName))
    val tghas = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)

    val newContent = "newContent"
    val javaEditor = SimpleFileEditor(_.name.endsWith(".java"), f => StringFileArtifact(f.path, newContent))
    val edited = tghas ✎ javaEditor
    edited.allFiles should have size 10
    edited.findFile("src/main/java/com/example/DemoApplication.java").get.content should equal(newContent)
  }
}

class MarkdownFilter extends ArtifactFilter {

  override def apply(s: String): Boolean = !s.endsWith(".md")
}
