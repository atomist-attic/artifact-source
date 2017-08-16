package com.atomist.source.git

import com.atomist.source._
import com.atomist.source.file.NamedFileSystemArtifactSourceIdentifier
import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch
import com.atomist.source.git.TestConstants._
import com.atomist.source.git.domain.ReactionContent._
import com.atomist.source.git.domain.{PullRequest, PullRequestRequest, ReactionContent}
import org.apache.commons.codec.binary.Base64

class GitHubServicesTest extends GitHubMutatorTest(Token) {

  private val grc = GitRepositoryCloner(Token)

  "GitHubServices" should "search repositories by repo owner and name" in {
    val repos = ghs.searchRepositories(Map("q" -> s"repo:atomist/artifact-source"))
    repos.items.size shouldEqual 1
  }

  it should "list all branches" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val branchName = "foobar"
    val ref = ghs.createBranch(repo, owner, branchName, MasterBranch)
    ref.`object`.sha should not be empty

    val branches = ghs.listBranches(repo, owner)
    branches.size shouldBe 2
  }

  it should "create and delete branch" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val branchName = "foobar"
    val ref = ghs.createBranch(repo, owner, branchName, MasterBranch)
    ref.`object`.sha should not be empty

    ghs.deleteBranch(repo, owner, branchName)
    ghs.getBranch(repo, owner, branchName) shouldBe empty
    ghs.listBranches(repo, owner) should have size 1 // "master"
  }

  it should "fail to find unknown branch" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    ghs.getBranch(repo, owner, "foobar") shouldBe empty
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
    val ref = ghs.createBranch(repo, owner, newBranchName, MasterBranch)
    ref.`object`.sha should not be empty
    val newBranchSource = GitHubArtifactSourceLocator(cri, newBranchName)

    val tempFiles = createTempFiles(newBranchSource)
    val filesToDelete = tempFiles :+ StringFileArtifact(placeholderFilename("bogusFile"), testFileContents)

    val files = testFiles :+ StringFileArtifact("somethingOrOther.txt", testFileContents) // Duplicate
    val multiFileCommitMessage = s"multi file commit at ${System.currentTimeMillis}"
    val fileCommit2 = ghs.commitFiles(
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
    val commitsAfter = ghs.listCommits(repo, owner, Some(sha))
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
    val pr = ghs.createPullRequestFromChanges(GitHubArtifactSourceLocator(cri, MasterBranch).repo,
      GitHubArtifactSourceLocator(cri, MasterBranch).owner, prr, startAs, withAddedFiles, message)
    pr.number should be > 0
    pr.isOpen shouldBe true
    pr.body should equal(prBody)
    pr.htmlUrl.length should be > 0

    val rc = ghs.createPullRequestReviewComment(repo, owner, pr.number, "comment body", pr.head.sha, "somethingOrOther.txt", 1)
    rc.body should equal("comment body")

    val openPrs = ghs.listPullRequests(repo, owner)
    openPrs.isEmpty shouldBe false
    openPrs.map(_.title) should contain(prTitle)

    val pr2 = ghs.getPullRequest(repo, owner, pr.number)
    pr2 shouldBe defined
    val pr1 = pr2.get
    pr1.number should equal(pr.number)
    pr1.base.ref should equal(prr.base)
    pr1.head.ref should equal(prr.head)
    pr1.htmlUrl should not be null

    val merged = ghs.mergePullRequest(repo, owner, pr1.number, pr1.title, "Merged PR")
    merged.merged shouldBe true

    val pr3 = ghs.getPullRequest(repo, owner, pr1.number)
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
    val cloned = grc.clone(repo, owner)
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
    val pr = ghs.createPullRequestFromChanges(repo, owner, prr, startAs, editedAgain, "Added files")

    val merged = ghs.mergePullRequest(repo, owner, pr.number, pr.title, "Merged PR")
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

    ghs.createBranch(repo, owner, newBranchName, MasterBranch)
    ghs.addOrUpdateFile(repo, owner, newBranchName, "new file 3", StringFileArtifact("alan.txt", "alan stewart"))

    populateAndVerify(repo, owner, newBranchName)
  }

  it should "list all commits in a repository" in {
    val commits = ghs.listCommits("artifact-source", "atomist")
    commits.size should be > 200
  }

  it should "search commits with search criteria" in {
    val results = ghs.searchCommits(Map("q" -> s"repo:atomist/artifact-source logging", "per_page" -> "10"))
    results.items.size shouldEqual results.items.groupBy(_.sha).map(_._2.head).size
  }

  it should "search commits with specified page number" in {
    val results = ghs.searchCommits(Map("q" -> s"repo:atomist/artifact-source logging", "per_page" -> "10", "page" -> "1"))
    results.items.size shouldEqual 10
  }

  it should "get all pull requests in a repository" in {
    val pullRequests = ghs.listPullRequests("rug", "atomist", PullRequest.All)
    pullRequests.size should be > 300
  }

  it should "create and test webhook in a repository" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val url = "http://example.com/webhook"
    val webhook = ghs.createWebhook(repo, owner, "web", url, "json", active = true, Array("push", "pull_request"))
    webhook.name should equal("web")
    webhook.config.url should equal(url)
    webhook.id should be > 0
    webhook.active shouldBe true
    webhook.events should contain allOf("push", "pull_request")

    ghs.testWebhook(repo, owner, webhook.id)
    ghs.deleteWebhook(repo, owner, webhook.id)
  }

  it should "create webhook in an organization" in {
    val url = "http://example.com/webhook"
    val org = "atomisthqtest"
    ghs.listOrganizationWebhooks(org)
      .find(_.config.url == url)
      .foreach(wh => ghs.deleteOrganizationWebhook(org, wh.id))

    val webhook = ghs.createOrganizationWebhook(org, "web", url, "json", active = true, Array("*"))
    webhook.name should equal("web")
    webhook.config.url should equal(url)
    webhook.id should be > 0
    webhook.active shouldBe true
    webhook.events should contain only "*"
    ghs.deleteOrganizationWebhook(org, webhook.id)
  }

  it should "fail to create duplicate webhook in an organization" in {
    val url = "http://example.com/webhook"
    val org = "atomisthqtest"
    ghs.listOrganizationWebhooks(org)
      .find(_.config.url == url)
      .foreach(wh => ghs.deleteOrganizationWebhook(org, wh.id))

    val webhook = ghs.createOrganizationWebhook(org, "web", url, "json", active = true, Array("*"))
    an[ArtifactSourceException] should be thrownBy ghs.createOrganizationWebhook(org, "web", url, "json", active = true, Array("*"))
    ghs.deleteOrganizationWebhook(org, webhook.id)
  }

  it should "add a collaborator to a repository" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    ghs.addCollaborator(repo, owner, "alankstewart")
  }

  it should "create, edit issue, and create issue comment in a repository" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val issue = ghs.createIssue(repo, owner, "issue 1", "issue body", Seq("bug"))
    issue.number should be > 0
    issue.labels.length should be > 0
    issue.state shouldBe "open"
    issue.closedAt shouldBe empty

    val retrievedIssue = ghs.getIssue(repo, owner, issue.number)
    retrievedIssue shouldBe defined
    val iss = retrievedIssue.get

    ghs.createIssueReaction(repo, owner, iss.number, Heart)

    val comment = ghs.createIssueComment(repo, owner, iss.number, "issue comment 1")
    comment.body shouldEqual "issue comment 1"

    ghs.createIssueCommentReaction(repo, owner, comment.id, PlusOne)

    val editedIssue = ghs.editIssue(repo, owner, iss.number, iss.title, iss.body, state = "closed",
      labels = Seq("bug", "feature"), assignees = Seq("alankstewart"))
    editedIssue.state shouldBe "closed"
    editedIssue.closedAt shouldBe defined
    editedIssue.assignees should have size 1

    val issues = ghs.listIssues()
    issues.size should be > 0

    val results = ghs.searchIssues(Map("q" -> s"repo:$owner/$repo state:closed", "per_page" -> "100", "page" -> "1"))
    results.items.size should be > 0
  }

  it should "list issues with search criteria" in {
    val results = ghs.listIssues(Map("per_page" -> "30", "sort" -> "updated", "direction" -> "asc", "state" -> "closed"))
    results.size should be > 10
  }

  it should "list issues with specified page number" in {
    val results = ghs.listIssues(Map("per_page" -> "10", "sort" -> "updated", "direction" -> "asc", "page" -> "1"))
    results.size shouldEqual 10
  }

  it should "search issues with search criteria" in {
    val results = ghs.searchIssues(Map("q" -> s"repo:atomist/artifact-source", "per_page" -> "10"))
    results.items.size shouldEqual results.items.groupBy(_.number).map(_._2.head).size
  }

  it should "search issues with specified page number" in {
    val results = ghs.searchIssues(Map("q" -> s"repo:atomist/artifact-source", "per_page" -> "10", "page" -> "2"))
    results.items.size shouldEqual 10
  }

  it should "create commit comment and reaction" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val commits = ghs.listCommits(repo, owner)
    commits.size should be > 0
    val commit = commits.head
    val commitComment = ghs.createCommitComment(repo, owner, commit.sha, "test comment", "README.md", 1)
    commitComment.body shouldEqual "test comment"

    val reaction = ghs.createCommitCommentReaction(repo, owner, commitComment.id, ReactionContent.PlusOne)
    reaction.content shouldEqual ReactionContent.PlusOne

    val reactions = ghs.listCommitCommentReactions(repo, owner, commitComment.id, Some(ReactionContent.PlusOne))
    reactions.size shouldEqual 1
    reactions.head.content shouldEqual ReactionContent.PlusOne
  }

  it should "get file contents" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.name
    val owner = newTempRepo.ownerName

    val readme = ghs.getFileContents(repo, owner, "README.md")
    readme should have size 1
    new String(Base64.decodeBase64(readme.head.content)) should include("temporary test repository")
  }

  private def createTempFiles(newBranchSource: GitHubArtifactSourceLocator): Seq[FileArtifact] = {
    val files: Seq[FileArtifact] = Seq(
      StringFileArtifact(placeholderFilename("tempFile1"), testFileContents),
      StringFileArtifact(placeholderFilename("tempFile2"), testFileContents),
      StringFileArtifact(placeholderFilename("tempFile3"), testFileContents)
    )
    val multiFileCommitMessage = s"multi temp file commit at ${System.currentTimeMillis}"
    val fileArtifacts = ghs.commitFiles(GitHubSourceUpdateInfo(newBranchSource, multiFileCommitMessage), files, Seq.empty)
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
    val pr = ghs.createPullRequestFromChanges(repo, owner, prr, startAs, modifiedAs, multiFileCommitMessage)
    println(s"Elapsed time to create pull request from deltas = ${System.currentTimeMillis() - start} ms")

    ghs.createPullRequestReviewComment(repo, owner, pr.number, "comment body", pr.head.sha, "test2.json", 1)

    val merged = ghs.mergePullRequest(repo, owner, pr.number, pr.title, "Merged PR")
    merged.merged shouldBe true

    val endAs = ghs sourceFor GitHubArtifactSourceLocator(cri)
    endAs.findFile("src/test.txt") shouldBe empty
    endAs.findFile("test.json").map(_.content shouldEqual newContent)
      .getOrElse(fail("expected test.json but not found"))
  }
}
