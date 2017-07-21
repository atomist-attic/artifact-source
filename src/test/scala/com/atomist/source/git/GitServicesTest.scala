package com.atomist.source.git

import com.atomist.source._
import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch
import com.atomist.source.git.TestConstants.Token
import com.atomist.source.git.github.domain.PullRequestRequest
import com.atomist.source.git.github.{GitHubArtifactSourceLocator, GitHubMutatorTest}

class GitServicesTest extends GitHubMutatorTest(Token) {

  private val gs = GitServices(Token)

  "GitServices" should "create and edit a new file, and create a pull request" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val cri = SimpleCloudRepoId(repo, owner)
    val startAs = ghs sourceFor GitHubArtifactSourceLocator(cri, branch = MasterBranch)

    val start = System.currentTimeMillis()

    val path1 = "test.json"
    val newFile1 = StringFileArtifact(path1, "test content")
    val as = startAs + newFile1 + StringFileArtifact("test2.json", "test content 2")

    val newContent = "new content"
    val stringEditor = SimpleFileEditor(_.name == path1, f => StringFileArtifact(f.path, newContent))
    val edited = as ✎ stringEditor
    edited should not be theSameInstanceAs(as)
    edited.findFile(path1).get.content should equal(newContent)

    val newContent1 = "new content 1"
    val stringEditor1 = SimpleFileEditor(_.name == path1, f => StringFileArtifact(f.path, newContent1))
    val editedAgain = edited ✎ stringEditor1
    editedAgain should not be theSameInstanceAs(edited)
    editedAgain.findFile(path1).get.content should equal(newContent1)

    val prTitle = s"My pull request at ${System.currentTimeMillis}"
    val prBody = "This is the body of my pull request"
    val updatedBranch = s"multi-file-${System.currentTimeMillis}"
    val prr = PullRequestRequest(prTitle, updatedBranch, MasterBranch, prBody)

    val pr = gs createPullRequestFromChanges(repo, owner, prr, startAs, editedAgain, "Added files")
    val merged = ghs mergePullRequest(repo, owner, pr.number, pr.title, "Merged PR")
    merged.merged shouldBe true
    println(s"Elapsed time = ${System.currentTimeMillis() - start} ms")

    val newAs = ghs sourceFor GitHubArtifactSourceLocator.rootOfMaster(repo, owner)
    val f1 = newAs.findFile(path1)
    f1 shouldBe defined
    f1.get.content shouldEqual newContent1
  }

  it should "clone repo, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName
    createContent(repo, owner)

    val newBranchName = "add-multi-files-branch"
    populateAndVerify(repo, owner, newBranchName)
  }

  it should "clone repo, add, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName
    createContent(repo, owner)

    val newBranchName = "add-multi-files-branch"
    ghs createBranch(repo, owner, newBranchName, MasterBranch)
    ghs addFile(repo, owner, newBranchName, "new file 3", StringFileArtifact("alan.txt", "alan stewart"))

    populateAndVerify(repo, owner, newBranchName)
  }

  private def populateAndVerify(repo: String, owner: String, newBranchName: String) = {
    val cri = SimpleCloudRepoId(repo, owner)
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

    val prTitle = s"My pull request at ${System.currentTimeMillis}"
    val prBody = "This is the body of my pull request"
    val prr = PullRequestRequest(prTitle, newBranchName, MasterBranch, prBody)

    val start = System.currentTimeMillis
    val pr = gs createPullRequestFromChanges(repo, owner, prr, startAs, modifiedAs, multiFileCommitMessage)
    println(s"Elapsed time to create pull request from deltas = ${System.currentTimeMillis() - start} ms")

    val merged = ghs mergePullRequest(repo, owner, pr.number, pr.title, "Merged PR")
    merged.merged shouldBe true

    val endAs = ghs sourceFor GitHubArtifactSourceLocator(cri)
    endAs.findFile("src/test.txt") shouldBe empty
    endAs.findFile("test.json").map(_.content shouldEqual newContent)
      .getOrElse(fail("expected test.json but not found"))
  }
}
