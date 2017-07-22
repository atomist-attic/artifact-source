package com.atomist.source.git.github

import com.atomist.source._
import com.atomist.source.file.NamedFileSystemArtifactSourceIdentifier
import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch
import com.atomist.source.git.TestConstants._
import com.atomist.source.git.github.domain.{PullRequest, PullRequestRequest, Webhook}
import com.atomist.source.git.{FileSystemGitArtifactSource, GitRepositoryCloner}

class GitHubServicesTest extends GitHubMutatorTest(Token) {

  private val grc = GitRepositoryCloner(Token)

  "GitHubServices" should "find valid organization" in {
    ghs getOrganization TestOrg shouldBe defined
  }

  it should "fail to find unknown organization" in {
    ghs getOrganization "comfoobar" shouldBe empty
  }

  it should "find valid user repository" in {
    ghs getRepository("satin-scala", "alankstewart") shouldBe defined
  }

  it should "fail to find unknown user repository" in {
    ghs getRepository("comfoobar", "alankstewart") shouldBe empty
  }

  it should "fail to find unknown organization repository" in {
    ghs getRepository("unknown-repo", TestOrg) shouldBe empty
  }

  it should "handle null repository" in {
    ghs.getRepository(null, TestOrg) shouldBe empty
  }

  it should "handle empty repository" in {
    ghs.getRepository(" ", TestOrg) shouldBe empty
  }

  it should "handle null organization" in {
    ghs.getRepository("unknown-repo", null) shouldBe empty
  }

  it should "handle empty organization" in {
    ghs.getRepository("unknown-repo", "") shouldBe empty
  }

  it should "delete files with valid path in multi file commit" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val cri = SimpleCloudRepoId(repo, owner)
    val master = GitHubArtifactSourceLocator(cri, branch = MasterBranch)
    val before = ghs sourceFor master
    before.allFiles should have size 1

    val newBranchName = "add-multi-files-branch"
    val ref = ghs createBranch(repo, owner, newBranchName, MasterBranch)
    ref.`object`.sha should not be empty
    val newBranchSource = GitHubArtifactSourceLocator(cri, newBranchName)

    val tempFiles = createTempFiles(newBranchSource)
    val filesToDelete = tempFiles :+ StringFileArtifact(placeholderFilename("bogusFile"), testFileContents)

    val files = testFiles :+ StringFileArtifact("somethingOrOther.txt", testFileContents) // Duplicate
    val multiFileCommitMessage = s"multi file commit at ${System.currentTimeMillis}"
    val fileCommit2 = ghs commitFiles(
      GitHubSourceUpdateInfo(newBranchSource, multiFileCommitMessage), files, filesToDelete)
    fileCommit2.isEmpty shouldBe false

    val masterRead = ghs sourceFor master
    masterRead.allFiles.size should equal(before.allFiles.size)

    val branchRead = ghs sourceFor newBranchSource
    branchRead.allFiles.size should equal(files.size)
    for (f <- files)
      withClue(s"should find newly added file $f") {
        branchRead.findFile(f.path) shouldBe defined
        branchRead.findFile(f.path).get.content should equal(f.content)
      }

    val sha = branchRead.id.asInstanceOf[GitHubArtifactSourceIdentifier].commitSha
    val commitsAfter = ghs.getCommits(repo, owner, Some(sha))
    withClue(s"${files.size} files should have been added in single commit") {
      commitsAfter should have size 3
    }
    commitsAfter.map(_.commit.message) should contain(multiFileCommitMessage)
  }

  it should "create new branch and create and delete pull request via create pull request from delta" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName
    createContent(repo, owner)

    val cri = SimpleCloudRepoId(repo, owner)
    val startAs = ghs sourceFor GitHubArtifactSourceLocator(cri, MasterBranch)
    startAs.empty shouldBe false
    val modifiedAs = startAs delete "README.md" delete "src/test.txt"

    val files = testFiles :+ StringFileArtifact("src/test2.txt", "New content")
    val withAddedFiles = modifiedAs + files

    (withAddedFiles Δ modifiedAs).deltas.isEmpty shouldBe false

    val prTitle = s"My pull request at ${System.currentTimeMillis}"
    val prBody = "This is the body of my pull request"
    val updatedBranch = s"multi-file-${System.currentTimeMillis}"
    val prr = PullRequestRequest(prTitle, updatedBranch, MasterBranch, prBody)
    val message = "Added files and deleted files including README.md"
    val pr = ghs createPullRequestFromChanges(GitHubArtifactSourceLocator(cri, MasterBranch).repo,
      GitHubArtifactSourceLocator(cri, MasterBranch).owner, prr, startAs, withAddedFiles, message)
    pr.number should be > 0
    pr.isOpen shouldBe true
    pr.body should equal(prBody)
    pr.htmlUrl.length should be > 0

    val rc = ghs createReviewComment(repo, owner, pr.number, "comment body", pr.head.sha, "somethingOrOther.txt", 1)
    rc.body should equal("comment body")

    val openPrs = ghs getPullRequests(repo, owner)
    openPrs.isEmpty shouldBe false
    openPrs.map(_.title) should contain(prTitle)

    val pr2 = ghs getPullRequest(repo, owner, pr.number)
    pr2 shouldBe defined
    val pr1 = pr2.get
    pr1.number should equal(pr.number)
    pr1.base.ref should equal(prr.base)
    pr1.head.ref should equal(prr.head)
    pr1.htmlUrl should not be null

    val merged = ghs mergePullRequest(repo, owner, pr1.number, pr1.title, "Merged PR")
    merged.merged shouldBe true

    val pr3 = ghs getPullRequest(repo, owner, pr1.number)
    pr3 shouldBe defined
    pr3.get.merged shouldBe true

    val endAs = ghs sourceFor GitHubArtifactSourceLocator(cri, MasterBranch)
    endAs.allFiles.size shouldBe 4
  }

  it should "clone remote repository, create and edit a new file, and create a pull request" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val start = System.currentTimeMillis()
    val cloned = grc clone(repo, owner) match {
      case Left(e) => fail(e)
      case Right(repoDir) => repoDir
    }

    val startAs = FileSystemGitArtifactSource(NamedFileSystemArtifactSourceIdentifier(repo, cloned))

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
    val pr = ghs createPullRequestFromChanges(repo, owner, prr, startAs, editedAgain, "Added files")

    val merged = ghs mergePullRequest(repo, owner, pr.number, pr.title, "Merged PR")
    merged.merged shouldBe true
    println(s"Elapsed time = ${System.currentTimeMillis() - start} ms")

    val newAs = ghs sourceFor GitHubArtifactSourceLocator.rootOfMaster(repo, owner)
    val f1 = newAs.findFile(path1)
    f1 shouldBe defined
    f1.get.content shouldEqual newContent1
  }

  it should "create repo, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName
    createContent(repo, owner)

    val newBranchName = "add-multi-files-branch"
    populateAndVerify(repo, owner, newBranchName)
  }

  it should "create repo, add, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName
    createContent(repo, owner)

    val newBranchName = "add-multi-files-branch"

    ghs createBranch(repo, owner, newBranchName, MasterBranch)
    ghs addFile(repo, owner, newBranchName, "new file 3", StringFileArtifact("alan.txt", "alan stewart"))

    populateAndVerify(repo, owner, newBranchName)
  }

  it should "get all commits in a repository" in {
    val commits = ghs getCommits("artifact-source", "atomist")
    commits.size should be > 200
  }

  it should "get all pull requests in a repository" in {
    val pullRequests = ghs getPullRequests("rug", "atomist", PullRequest.All)
    pullRequests.size should be > 300
  }

  it should "create and test webhook in a repository" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val wh = Webhook("web", "http://webhook.site/10ceed7a-7128-4b11-bc8c-364198f065c9", "json", Seq("push"))
    val webhook = ghs createWebhook(repo, owner, wh)
    webhook.name should equal(wh.name)
    webhook.config.url should equal(wh.config.url)
    webhook.id should be > 0
    webhook.active shouldBe true
    webhook.events should contain only "push"

    ghs.testWebhook(repo, owner, webhook.id)
  }

  ignore should "create webhook in an organization" in {
    val wh = Webhook("web", "http://webhook.site/10ceed7a-7128-4b11-bc8c-364198f065c9", "json", Seq("push"))
    val webhook = ghs createOrganizationWebhook("atomist", wh)
    webhook.name should equal(wh.name)
    webhook.config.url should equal(wh.config.url)
    webhook.id should be > 0
    webhook.active shouldBe true
    webhook.events should contain only "push"
  }

  it should "add a collaborator to a repository" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    ghs addCollaborator(repo, owner, "alankstewart")

  }

  private def createTempFiles(newBranchSource: GitHubArtifactSourceLocator): Seq[FileArtifact] = {
    val files: Seq[FileArtifact] = Seq(
      StringFileArtifact(placeholderFilename("tempFile1"), testFileContents),
      StringFileArtifact(placeholderFilename("tempFile2"), testFileContents),
      StringFileArtifact(placeholderFilename("tempFile3"), testFileContents)
    )
    val multiFileCommitMessage = s"multi temp file commit at ${System.currentTimeMillis}"
    val fileArtifacts = ghs commitFiles(GitHubSourceUpdateInfo(newBranchSource, multiFileCommitMessage), files, Seq.empty)
    fileArtifacts.isEmpty shouldBe false
    fileArtifacts
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
    val pr = ghs createPullRequestFromChanges(repo, owner, prr, startAs, modifiedAs, multiFileCommitMessage)
    println(s"Elapsed time to create pull request from deltas = ${System.currentTimeMillis() - start} ms")

    val merged = ghs mergePullRequest(repo, owner, pr.number, pr.title, "Merged PR")
    merged.merged shouldBe true

    val endAs = ghs sourceFor GitHubArtifactSourceLocator(cri)
    endAs.findFile("src/test.txt") shouldBe empty
    endAs.findFile("test.json").map(_.content shouldEqual newContent)
      .getOrElse(fail("expected test.json but not found"))
  }
}
