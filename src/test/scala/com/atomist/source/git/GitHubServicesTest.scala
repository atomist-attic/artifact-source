package com.atomist.source.git

import com.atomist.source._
import com.atomist.source.file.NamedFileSystemArtifactSourceIdentifier
import com.atomist.source.git.GitHubArtifactSourceLocator.MasterBranch
import com.atomist.source.git.GitHubServices.PullRequestRequest
import com.atomist.source.git.TestConstants._
import com.atomist.util.GitRepositoryCloner
import org.kohsuke.github.{GHIssueState, GHRepository}

import scala.collection.JavaConverters._

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
    an[IllegalArgumentException] should be thrownBy ghs.getRepository(null, TestOrg)
  }

  it should "handle empty repository" in {
    an[IllegalArgumentException] should be thrownBy ghs.getRepository(" ", TestOrg)
  }

  it should "handle null organization" in {
    an[IllegalArgumentException] should be thrownBy ghs.getRepository("unknown-repo", null)
  }

  it should "handle empty organization" in {
    an[IllegalArgumentException] should be thrownBy ghs.getRepository("unknown-repo", "")
  }

  it should "delete files with valid path in multi file commit" in {
    val newTempRepo = newPopulatedTemporaryRepo()

    val cri = SimpleCloudRepoId(newTempRepo.getName, newTempRepo.getOwnerName)
    val master = GitHubArtifactSourceLocator(cri, branch = MasterBranch)
    val before = ghs sourceFor master
    before.allFiles should have size 1

    val newBranchName = "add-multi-files-branch"
    val gHRef = ghs createBranch(newTempRepo, newBranchName, MasterBranch)
    gHRef.getObject.getSha should not be empty
    val newBranchSource = GitHubArtifactSourceLocator(cri, newBranchName)

    val tempFiles = createTempFiles(newBranchSource)
    val filesToDelete = tempFiles :+ StringFileArtifact(placeholderFilename("bogusFile"), testFileContents)

    val files = testFiles :+ StringFileArtifact("somethingOrOther.txt", testFileContents) // Duplicate
    val multiFileCommitMessage = s"multi file commit at ${System.currentTimeMillis}"
    val fileCommit2 = ghs commitFiles(
      GitHubSourceUpdateInfo(newBranchSource, multiFileCommitMessage), files.asJava, filesToDelete.asJava)
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
    val commitsAfter = newTempRepo.queryCommits().from(sha).list().asList().asScala
    withClue(s"${files.size} files should have been added in single commit") {
      commitsAfter should have size 3
    }
    commitsAfter.map(_.getCommitShortInfo.getMessage) should contain(multiFileCommitMessage)
  }

  it should "create new branch and create and delete pull request via create pull request from delta" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    newTempRepo.createContent("some text".getBytes, "new file 1", "src/test.txt", "master")
    newTempRepo.createContent("some other text".getBytes, "new file 2", "src/test2.txt", "master")

    val repo = newTempRepo.getName
    val owner = newTempRepo.getOwnerName
    val cri = SimpleCloudRepoId(repo, owner)
    val prTitle = s"My pull request at ${System.currentTimeMillis}"
    val prBody = "This is the body of my pull request"
    val updatedBranch = s"multi-file-${System.currentTimeMillis}"
    val baseSource = GitHubArtifactSourceLocator(cri, MasterBranch)

    val startAs = ghs sourceFor baseSource
    startAs.empty shouldBe false
    val modifiedAs = startAs delete "README.md" delete "src/test.txt"

    val files = testFiles :+ StringFileArtifact("src/test2.txt", "New content")
    val withAddedFiles = modifiedAs + files

    (withAddedFiles Δ modifiedAs).deltas.isEmpty shouldBe false

    val prr = PullRequestRequest(prTitle, updatedBranch, MasterBranch, prBody)
    val prs = ghs createPullRequestFromChanges(baseSource.repo, baseSource.owner, prr, startAs, withAddedFiles, "Added files and deleted files including README.md")
    prs shouldBe defined
    val pr0 = prs.get
    pr0.number should be > 0
    pr0.isOpen shouldBe true
    pr0.body should equal(prBody)
    pr0.htmlUrl.length should be > 0

    val rc = ghs createReviewComment(repo, owner, pr0.number, "comment body", pr0.head.sha, "somethingOrOther.txt", 1)
    rc.body should equal("comment body")

    val openPrs = newTempRepo.getPullRequests(GHIssueState.OPEN)
    openPrs.isEmpty shouldBe false
    openPrs.asScala.map(_.getTitle) should contain(prTitle)

    val pr1 = newTempRepo.getPullRequest(pr0.number)
    pr1.getNumber should equal(pr0.number)
    pr1.getBase.getRef should equal(prr.base)
    pr1.getHead.getRef should equal(prr.head)
    pr1.getHtmlUrl should not be null

    val merged = ghs mergePullRequest(repo, owner, pr1.getNumber, pr1.getTitle, "Merged PR")
    merged shouldBe defined
    merged.get.merged shouldBe true

    val pr2 = newTempRepo.getPullRequest(pr1.getNumber)
    pr2.isMerged shouldBe true

    val endAs = ghs sourceFor baseSource
    endAs.allFiles.size shouldBe 4
  }

  it should "clone remote repository, create and edit a new file, and create a pull request" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    val repo = newTempRepo.getName
    val owner = newTempRepo.getOwnerName

    val start = System.currentTimeMillis()
    val cloned = grc clone(repo, owner) match {
      case Left(e) => fail(e)
      case Right(repoDir) => repoDir
    }
    val id = NamedFileSystemArtifactSourceIdentifier(repo, cloned)
    val startAs = FileSystemGitArtifactSource(id)

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
    val prs = ghs createPullRequestFromChanges(repo, owner, prr, startAs, editedAgain, "Added files")
    prs shouldBe defined
    val pr1 = prs.get

    val merged = ghs mergePullRequest(repo, owner, pr1.number, pr1.title, "Merged PR")
    merged shouldBe defined
    merged.get.merged shouldBe true
    println(s"Elapsed time = ${System.currentTimeMillis() - start} ms")

    val newAs = ghs sourceFor GitHubArtifactSourceLocator.rootOfMaster(repo, owner)
    val f1 = newAs.findFile(path1)
    f1 shouldBe defined
    f1.get.content shouldEqual newContent1
  }

  it should "clone repo, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    createContent(newTempRepo)

    val newBranchName = "add-multi-files-branch"
    populateAndVerify(newTempRepo.getName, newTempRepo.getOwnerName, newBranchName, MasterBranch)
  }

  it should "create repo, add, update, delete files, and create a pull request from deltas" in {
    val newTempRepo = newPopulatedTemporaryRepo()
    createContent(newTempRepo)

    val newBranchName = "add-multi-files-branch"

    ghs.createBranch(newTempRepo, newBranchName, MasterBranch)
    newTempRepo.createContent("alan stewart".getBytes, "new file 3", "alan.txt", newBranchName)

    populateAndVerify(newTempRepo.getName, newTempRepo.getOwnerName, newBranchName, newBranchName)
  }

  private def createContent(newTempRepo: GHRepository) = {
    newTempRepo.createContent("some text".getBytes, "new file 1", "src/test.txt", MasterBranch)
    newTempRepo.createContent("some other text".getBytes, "new file 2", "src/test2.txt", MasterBranch)
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

  private def populateAndVerify(repo: String, ownwer: String, newBranchName: String, fromBranch: String) = {
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
    ghs.createBranchFromChanges(repo, ownwer, newBranchName, fromBranch, startAs, modifiedAs, multiFileCommitMessage)
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
