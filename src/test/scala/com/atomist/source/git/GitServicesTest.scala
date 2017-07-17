package com.atomist.source.git

import com.atomist.source._
import com.atomist.source.file.NamedFileSystemArtifactSourceIdentifier
import com.atomist.source.git.GitHubArtifactSourceLocator.MasterBranch
import com.atomist.source.git.GitHubServices.PullRequestRequest
import com.atomist.source.git.TestConstants.Token

class GitServicesTest extends GitHubMutatorTest(Token) {

  private val (repo, owner) = ("github-service", "atomisthq")

  "GitServices" should "clone remote repo" in {
    val gs = GitServices(Token)
    val start = System.currentTimeMillis()
    val repository = gs.clone(repo, owner)
    println(s"Elapsed time = ${System.currentTimeMillis() - start} ms")
    repository should not be null
  }

  it should "clone remote repo with git command" in {
    val gs = GitServices(Token)
    val start = System.currentTimeMillis()
    gs.cloneCmd(repo, owner)
    println(s"Elapsed time = ${System.currentTimeMillis() - start} ms")
  }

  it should "delete files with valid path in multi file commit" in pendingUntilFixed {
    val newTempRepo = newPopulatedTemporaryRepo()
    newTempRepo.createContent("some text".getBytes, "new file 1", "src/test.txt", MasterBranch)
    newTempRepo.createContent("some other text".getBytes, "new file 2", "src/test2.txt", MasterBranch)
    val newBranchName = "add-multi-files-branch"
    ghs.createBranch(newTempRepo, newBranchName, MasterBranch)
    newTempRepo.createContent("alan stewart".getBytes, "new file 3", "alan.txt", newBranchName)

    val start = System.currentTimeMillis
    val multiFileCommitMessage = s"multi file commit at ${System.currentTimeMillis}"

    val gs = GitServices(Token)
    gs.cloneCmd(newTempRepo.getName, newTempRepo.getOwnerName).map(f => {
      val startAs = FileSystemGitArtifactSource(NamedFileSystemArtifactSourceIdentifier("foo", f))

      val path1 = "test.json"
      val newFile1 = StringFileArtifact(path1, "test content")
      val as = startAs + newFile1 + StringFileArtifact("test2.json", "test content 2")
      val newContent = "new content"
      val stringEditor = SimpleFileEditor(_.name == path1, f => StringFileArtifact(f.path, newContent))
      val edited = as ✎ stringEditor
      edited should not be theSameInstanceAs(as)
      edited.findFile(path1).get.content should equal(newContent)

      val modifiedAs = edited delete "src/test.txt"
      gs.createBranchFromChanges(newTempRepo.getName, newTempRepo.getOwnerName, newBranchName, MasterBranch, startAs, modifiedAs, multiFileCommitMessage)

      val prTitle = s"My pull request at ${System.currentTimeMillis}"
      val prBody = "This is the body of my pull request"
      val prr = PullRequestRequest(prTitle, newBranchName, MasterBranch, prBody)
      val prs = ghs createPullRequest(newTempRepo.getName, newTempRepo.getOwnerName, prr, "Added files and deleted files")

      println("**** PR number = " + prs.number)

      val merged = ghs mergePullRequest(newTempRepo.getName, newTempRepo.getOwnerName, prs.number, prs.title, "Merged PR")
      merged shouldBe defined

      val cri = SimpleCloudRepoId(newTempRepo.getName, newTempRepo.getOwnerName)
      val tghas = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)
      val files = tghas.findFile("src/test.txt") shouldBe empty
    })
    println(s"Elapsed time = ${System.currentTimeMillis() - start} ms")
  }
}
