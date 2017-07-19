package com.atomist.source.git

import com.atomist.source._
import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch
import com.atomist.source.git.TestConstants.Token
import com.atomist.source.git.github.domain.PullRequestRequest
import com.atomist.source.git.github.{GitHubArtifactSourceLocator, GitHubMutatorTest, TreeGitHubArtifactSource}
import org.kohsuke.github.GHRepository

class GitServicesTest extends GitHubMutatorTest(Token) {

  private val grc = GitRepositoryCloner(Token)
  private val gs = GitServices(Token)

  "GitServices" should "clone repo, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    createContent(newTempRepo)

    val newBranchName = "add-multi-files-branch"
    populateAndVerify(newTempRepo.getName, newTempRepo.getOwnerName, newBranchName)
  }

  it should "clone repo, add, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    createContent(newTempRepo)

    val newBranchName = "add-multi-files-branch"

    ghs.createBranch(newTempRepo, newBranchName, MasterBranch)
    newTempRepo.createContent("alan stewart".getBytes, "new file 3", "alan.txt", newBranchName)

    populateAndVerify(newTempRepo.getName, newTempRepo.getOwnerName, newBranchName)
  }

  private def createContent(newTempRepo: GHRepository) = {
    newTempRepo.createContent("some text".getBytes, "new file 1", "src/test.txt", MasterBranch)
    newTempRepo.createContent("some other text".getBytes, "new file 2", "src/test2.txt", MasterBranch)
  }

  private def populateAndVerify(repo: String, ownwer: String, newBranchName: String) = {
    val cri = SimpleCloudRepoId(repo, ownwer)
    val master = GitHubArtifactSourceLocator(cri, branch = MasterBranch)
    val startAs = ghs sourceFor master

    val path = "test.json"
    val newFile = StringFileArtifact(path, "test content")
    val as = startAs + newFile + StringFileArtifact("test2.json", "test content 2")
    val newContent = "new content"
    val stringEditor = SimpleFileEditor(_.name == path, f => StringFileArtifact(f.path, newContent))
    val edited = as ✎ stringEditor
    edited should not be theSameInstanceAs(as)
    edited.findFile(path).get.content should equal(newContent)

    val modifiedAs = edited delete "src/test.txt"
    val multiFileCommitMessage = s"multi file commit at ${System.currentTimeMillis}"

    val start = System.currentTimeMillis
    gs.createBranchFromChanges(repo, ownwer, newBranchName, startAs, modifiedAs, multiFileCommitMessage)
    println(s"Elapsed time to create branch from deltas = ${System.currentTimeMillis() - start} ms")

    val prTitle = s"My pull request at ${System.currentTimeMillis}"
    val prBody = "This is the body of my pull request"
    val prr = PullRequestRequest(prTitle, newBranchName, MasterBranch, prBody)
    val prs = ghs createPullRequest(repo, ownwer, prr, "Added files and deleted files")

    val merged = ghs mergePullRequest(repo, ownwer, prs.number, prs.title, "Merged PR")
    merged shouldBe defined

    val tghas = TreeGitHubArtifactSource(GitHubArtifactSourceLocator(cri), ghs)
    tghas.findFile("src/test.txt") shouldBe empty
    tghas.findFile("test.json").map(_.content shouldEqual newContent)
      .getOrElse(fail("expected test.json but not found"))
  }
}
