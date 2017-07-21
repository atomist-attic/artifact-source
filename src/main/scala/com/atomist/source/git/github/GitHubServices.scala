package com.atomist.source.git.github

import java.io.InputStream
import java.nio.charset.Charset

import com.atomist.source.git.github.GitHubConstants.ApiUrl
import com.atomist.source.git.github.domain._
import com.atomist.source.{ArtifactSourceException, ArtifactSourceUpdateException, FileArtifact, _}
import com.atomist.util.JsonUtils.{fromJson, toJson}
import com.atomist.util.Octal.intToOctal
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.kohsuke.github.GitHub
import resource._

import scala.util.{Failure, Success, Try}
import scalaj.http.{Base64, Http}

case class GitHubServices(oAuthToken: String, apiUrl: Option[String] = None)
  extends GitHubSourceReader
    with LazyLogging {

  lazy val gitHub: GitHub = apiUrl.collect {
    case url if url != ApiUrl => GitHub.connectToEnterprise(url, oAuthToken)
  }.getOrElse(GitHub.connectUsingOAuth(oAuthToken))

  private val headers: Map[String, String] =
    Map(
      "Authorization" -> ("token " + oAuthToken),
      "Accept" -> "application/vnd.github.v3+json,application/vnd.github.loki-preview+json"
    )

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = TreeGitHubArtifactSource(id, this)

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = TreeGitHubArtifactSource(id, this)

  def getOrganization(owner: String): Option[Org] =
    Try(Http(s"$ApiUrl/orgs/$owner")
      .headers(headers)
      .execute(fromJson[Org])
      .throwError
      .body) match {
      case Success(org) => Some(org)
      case Failure(e) =>
        logger.warn(s"Failed to find organization $owner", e)
        None
    }

  def getRepository(repo: String, owner: String): Option[Repo] =
    Try(Http(s"$ApiUrl/repos/$owner/$repo")
      .headers(headers)
      .execute(fromJson[Repo])
      .throwError
      .body) match {
      case Success(r) => Some(r)
      case Failure(e) =>
        logger.warn(s"Failed to find user repository $owner/$repo. Searching for organization repository", e)
        val resp = Http(s"$ApiUrl/orgs/$owner/repos").headers(headers).execute(fromJson[Seq[Repo]])
        if (resp.is2xx)
          resp.body.find(_.name == repo)
        else {
          logger.warn(s"Failed to find user or organization repository $owner/$repo")
          None
        }
    }

  @throws[ArtifactSourceCreationException]
  def createRepository(repo: String,
                       owner: String,
                       description: String = "",
                       privateFlag: Boolean = false,
                       issues: Boolean = true,
                       autoInit: Boolean = false): Repo = {
    val url = getOrganization(owner).map(_ => s"$ApiUrl/orgs/$owner/repos").getOrElse(s"$ApiUrl/user/repos")
    val cr = CreateRepoRequest(repo, owner, description, privateFlag, issues, autoInit)
    Try(Http(url).postData(toJson(cr))
      .headers(headers)
      .execute(fromJson[Repo])
      .throwError
      .body) match {
      case Success(repository) => repository
      case Failure(e) => throw ArtifactSourceCreationException(e.getMessage, e)
    }
  }

  def createBranch(repo: String, owner: String, branchName: String, fromBranch: String): Reference = {
    Try(Http(s"$ApiUrl/repos/$owner/$repo/git/refs/heads/$fromBranch")
      .headers(headers)
      .execute(fromJson[Reference])
      .throwError
      .body) match {
      case Success(fromRef) =>
        val cr = CreateReference(s"refs/heads/$branchName", fromRef.`object`.sha)
        Http(s"$ApiUrl/repos/$owner/$repo/git/refs").postData(toJson(cr))
          .headers(headers)
          .execute(fromJson[Reference])
          .throwError
          .body
      case Failure(e) => throw ArtifactSourceCreationException(e.getMessage, e)
    }
  }

  @throws[ArtifactSourceUpdateException]
  def createBranchFromChanges(repo: String,
                              owner: String,
                              branchName: String,
                              fromBranch: String,
                              old: ArtifactSource,
                              current: ArtifactSource,
                              message: String): Seq[FileArtifact] =
    Try {
      if (branchName != fromBranch) {
        val ref = createBranch(repo, owner, branchName, fromBranch)
        logger.debug(ref.toString)
      }

      val deltas = current deltaFrom old
      val sui = GitHubSourceUpdateInfo(GitHubArtifactSourceLocator.fromStrings(repo, owner, branchName), message)

      val filesToUpdate = deltas.deltas.collect {
        case fud: FileUpdateDelta => fud.updatedFile
      }
      logger.debug(s"Updating files ${filesToUpdate.map(_.path).mkString(",")}")

      val filesToAdd = deltas.deltas.collect {
        case fad: FileAdditionDelta if !filesToUpdate.exists(_.path == fad.path) => fad.newFile
      }
      logger.debug(s"Adding files ${filesToAdd.map(_.path).mkString(",")}")
      val files = filesToUpdate ++ filesToAdd

      val filesToDelete = deltas.deltas.collect {
        case fdd: FileDeletionDelta => fdd.oldFile
      }
      logger.debug(s"Deleting files ${filesToDelete.map(_.path).mkString(",")}")

      commitFiles(sui, files, filesToDelete)
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def createPullRequestFromChanges(repo: String,
                                   owner: String,
                                   prr: PullRequestRequest,
                                   old: ArtifactSource,
                                   current: ArtifactSource,
                                   message: String): Option[PullRequestStatus] = {
    createBranchFromChanges(repo, owner, prr.head, prr.base, old, current, message)
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls").postData(toJson(prr))
      .headers(headers)
      .execute(fromJson[PullRequestStatus])
      .throwError
      .body) match {
      case Success(prs) => Some(prs)
      case Failure(e) => None
    }
  }

  @throws[ArtifactSourceUpdateException]
  def createPullRequest(repo: String,
                        owner: String,
                        prr: PullRequestRequest,
                        message: String): PullRequestStatus =
    Http(s"$ApiUrl/repos/$owner/$repo/pulls").postData(toJson(prr))
      .headers(headers)
      .execute(fromJson[PullRequestStatus])
      .throwError
      .body

  def getBranch(repo: String, owner: String, branch: String): Branch =
    Http(s"$ApiUrl/repos/$owner/$repo/branches/$branch")
      .headers(headers)
      .execute(fromJson[Branch])
      .throwError
      .body

  @throws[ArtifactSourceUpdateException]
  def createReviewComment(repo: String,
                          owner: String,
                          number: Int,
                          body: String,
                          commitId: String,
                          path: String,
                          position: Int): ReviewComment = {
    val crc = CreateReviewComment(body, commitId, path, position)
    logger.debug(crc.toString)
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number/comments").postData(toJson(crc))
      .headers(headers)
      .execute(fromJson[ReviewComment])
      .throwError
      .body) match {
      case Success(rc) => rc
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }
  }

  @throws[ArtifactSourceException]
  def commitFiles(sui: GitHubSourceUpdateInfo,
                  files: Seq[FileArtifact],
                  filesToDelete: Seq[FileArtifact]): Seq[FileArtifact] = {
    val sourceId = sui.sourceId
    val repo = sourceId.repo
    val owner = sourceId.owner
    commitFiles(repo, owner, sourceId.branch, sui.message, files, filesToDelete)
  }

  @throws[ArtifactSourceException]
  def commitFiles(repo: String,
                  owner: String,
                  branch: String,
                  message: String,
                  files: Seq[FileArtifact],
                  filesToDelete: Seq[FileArtifact]): Seq[FileArtifact] = {
    /*
      This process is involved, so here's a summary of what we need to do:
      1. Get a reference to the desired branch's HEAD
      2. Get the commit from the branch. We need the SHA.
      3. Post the new and updated files to the server as blobs, saving their SHAs
      4. Get the tree, recursively, that the commit points to from (2)
      5. Create a new tree containing the new and updated files, as well as omitting files to be deleted
      6. Create a new commit, referring to the tree from (5).
      7. Update our new branch's heads/master reference to point to our new commit from (6).
     */

    Try {
      val branchRef = getBranch(repo, owner, branch)
      val baseTreeSha = branchRef.commit.sha
      val fwbrs = files
        .groupBy(_.path)
        .map(_._2.last)
        .map(fa => FileWithBlobRef(fa, createBlob(repo, owner, message, branch, fa)))
        .toSeq

      val newOrUpdatedTreeEntries = fwbrs.map(fwbr => TreeEntry(fwbr.fa.path, intToOctal(fwbr.fa.mode), "blob", fwbr.ref.sha))
      val allExistingTreeEntries = treeFor(GitHubShaIdentifier(repo, owner, baseTreeSha))
        .allFiles
        .map(fa => TreeEntry(fa.path, intToOctal(fa.mode), "blob", fa.uniqueId.getOrElse("")))
      val treeEntriesToDelete = filesToDelete.map(fa => TreeEntry(fa.path, intToOctal(fa.mode), "blob", fa.uniqueId.getOrElse("")))

      val finalTreeEntries = (allExistingTreeEntries ++ newOrUpdatedTreeEntries)
        .groupBy(_.path)
        .map(_._2.last)
        .filterNot(te => treeEntriesToDelete.exists(_.path == te.path))
        .toSeq

      val tree = createTree(repo, owner, finalTreeEntries) match {
        case Left(msg) => throw new IllegalArgumentException(msg)
        case Right(ctr) => ctr
      }
      logger.debug(tree.toString)

      val commit = createCommit(repo, owner, message, tree, Seq(baseTreeSha))
      updateReference(repo, owner, s"heads/$branch", commit.sha)

      fwbrs.map(fwbr => fwbr.fa.withUniqueId(fwbr.ref.sha))
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to commit files to '$owner/$repo': ${e.getMessage}", e)
    }
  }

  @throws[ArtifactSourceUpdateException]
  def addFile(sui: GitHubSourceUpdateInfo, fa: FileArtifact): Option[FileArtifact] = {
    val sourceId = sui.sourceId
    val repo = sourceId.repo
    val owner = sourceId.owner
    addFile(repo, owner, sourceId.branch, sui.message, fa)
  }

  @throws[ArtifactSourceUpdateException]
  def addFile(repo: String,
              owner: String,
              branch: String,
              message: String,
              fa: FileArtifact): Option[FileArtifact] = {
    val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
    val cf = CreateFile(fa.path, message, content, branch)
    Try(Http(s"$ApiUrl/repos/$owner/$repo/contents/${fa.path}").postData(toJson(cf))
      .method("PUT")
      .headers(headers)
      .execute(fromJson[CreateFileResponse])
      .throwError
      .body) match {
      case Success(cfr) => Some(fa.withUniqueId(cfr.content.sha))
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to add file to $owner/$repo: ${e.getMessage}", e)
    }
  }

  @throws[ArtifactSourceUpdateException]
  def mergePullRequest(repo: String,
                       owner: String,
                       number: Int,
                       title: String,
                       message: String,
                       mergeMethod: String = "squash"): Option[PullRequestMerged] = {
    val prmr = PullRequestMergeRequest(title, message, mergeMethod)
    logger.debug(prmr.toString)
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number/merge").postData(toJson(prmr))
      .method("PUT")
      .headers(headers)
      .execute(fromJson[PullRequestMerged])
      .throwError
      .body) match {
      case Success(prm) => Some(prm)
      case Failure(e) =>
        logger.warn(s"Failed to merge pull request $number: ${e.getMessage}", e)
        None
    }
  }

  def getCommits(repo: String, owner: String, sha: Option[String] = None): Seq[Commit] = {
    val req = Http(s"$ApiUrl/repos/$owner/$repo/commits")
    sha.map(req.param("sha", _)).getOrElse(req)
      .headers(headers)
      .execute(fromJson[Seq[Commit]])
      .throwError
      .body
  }

  def getPullRequest(repo: String, owner: String, number: Int): Option[PullRequestStatus] =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number")
      .headers(headers)
      .execute(fromJson[PullRequestStatus])
      .throwError
      .body) match {
      case Success(pr) => Some(pr)
      case Failure(e) =>
        logger.warn(s"Failed to get pull request $number: ${e.getMessage}", e)
        None
    }

  def getPullRequests(repo: String,
                      owner: String,
                      state: Option[String] = Some(PullRequestStatus.OpenState)): Seq[PullRequestStatus] =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls")
      .param("state", state.get)
      .headers(headers)
      .execute(fromJson[Seq[PullRequestStatus]])
      .throwError
      .body) match {
      case Success(pullRequests) => pullRequests
      case Failure(e) =>
        logger.warn(s"Failed to get pull requests: ${e.getMessage}", e)
        Seq.empty
    }

  def getTreeRecursive(repo: String, owner: String, sha: String): Tree =
    Http(s"$ApiUrl/repos/$owner/$repo/git/trees/$sha")
      .param("recursive", "1")
      .headers(headers)
      .execute(fromJson[Tree])
      .throwError
      .body

  def readBlob(repo: String, owner: String, sha: String): InputStream =
    Http(s"$ApiUrl/repos/$owner/$repo/git/blobs/$sha")
      .header("Authorization", "token " + oAuthToken)
      .header("Accept", "application/vnd.github.v3.raw+json")
      .execute(IOUtils.toBufferedInputStream)
      .throwError
      .body

  private def createBlob(repo: String, owner: String, message: String, branch: String, fa: FileArtifact): GitHubRef = {
    val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
    val cbr = CreateBlobRequest(content)
    logger.debug(s"$branch,${fa.path},$cbr")
    Http(s"$ApiUrl/repos/$owner/$repo/git/blobs").postData(toJson(cbr))
      .headers(headers)
      .execute(fromJson[GitHubRef])
      .throwError
      .body
  }

  private def createTree(repo: String, owner: String, treeEntries: Seq[TreeEntry]): Either[String, CreateTreeResponse] = {
    val ctr = CreateTreeRequest(treeEntries)
    logger.debug(ctr.toString)
    Http(s"$ApiUrl/repos/$owner/$repo/git/trees").postData(toJson(ctr))
      .headers(headers)
      .exec((code: Int, headers: Map[String, IndexedSeq[String]], is: InputStream) => code match {
        case 201 => Right(fromJson[CreateTreeResponse](is))
        case _ => Left(s"${headers("Status").head}, ${IOUtils.toString(is, Charset.defaultCharset)}")
      }).body
  }

  private def createCommit(repo: String,
                           owner: String,
                           message: String,
                           tree: CreateTreeResponse,
                           parents: Seq[String]): CreateCommitResponse = {
    val cr = CreateCommitRequest(message, tree.sha, parents)
    logger.debug(cr.toString)
    Http(s"$ApiUrl/repos/$owner/$repo/git/commits").postData(toJson(cr))
      .headers(headers)
      .execute(fromJson[CreateCommitResponse])
      .throwError
      .body
  }

  private def updateReference(repo: String, owner: String, ref: String, newSha: String): Unit = {
    val urr = UpdateReferenceRequest(newSha, force = true)
    logger.debug(urr.toString)
    Http(s"$ApiUrl/repos/$owner/$repo/git/refs/$ref").postData(toJson(urr))
      .method("PATCH")
      .headers(headers)
      .asBytes
      .throwError
  }
}