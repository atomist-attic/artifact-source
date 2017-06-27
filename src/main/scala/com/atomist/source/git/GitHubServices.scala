package com.atomist.source.git

import java.time.OffsetDateTime
import java.util.{List => JList}

import com.atomist.source.{ArtifactSourceException, ArtifactSourceUpdateException, FileArtifact, StringFileArtifact, _}
import com.atomist.util.JsonUtils.{fromJson, toJson}
import com.atomist.util.Octal.intToOctal
import com.fasterxml.jackson.annotation.JsonProperty
import com.typesafe.scalalogging.LazyLogging
import org.kohsuke.github.{GHRef, GHRepository, GitHub, _}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scalaj.http.{Base64, Http}

case class GitHubServices(oAuthToken: String, apiUrl: String = "https://api.github.com")
  extends GitHubSourceReader
    with LazyLogging {

  import GitHubServices._

  lazy val gitHub: GitHub = apiUrl match {
    case url if Option(url).exists(_.trim.nonEmpty) && url != DefaultApiUrl => GitHub.connectToEnterprise(url, oAuthToken)
    case _ => GitHub.connectUsingOAuth(oAuthToken)
  }

  private val headers: Map[String, String] =
    Map("Authorization" -> ("token " + oAuthToken), "Accept" -> "application/vnd.github.v3+json")

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource =
    TreeGitHubArtifactSource(id, this)

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource =
    TreeGitHubArtifactSource(id, this)

  def getOrganization(owner: String): Option[GHOrganization] = {
    require(Option(owner).exists(_.trim.nonEmpty), "owner must not be null or empty")
    Try(gitHub.getOrganization(owner)) match {
      case Success(organization) => Some(organization)
      case Failure(e) =>
        logger.debug(e.getMessage, e)
        None
    }
  }

  def getRepository(repo: String, owner: String): Option[GHRepository] = {
    require(Option(repo).exists(_.trim.nonEmpty), "repo must not be null or empty")
    require(Option(owner).exists(_.trim.nonEmpty), "owner must not be null or empty")
    Try(gitHub.getRepository(s"$owner/$repo")) match {
      case Success(repository) => Option(repository)
      case Failure(e) =>
        logger.debug(e.getMessage, e)
        Option(getOrganization(owner).map(_.getRepository(repo)).orNull)
    }
  }

  @throws[ArtifactSourceCreationException]
  def createRepository(repo: String,
                       owner: String,
                       description: String = "",
                       privateFlag: Boolean = false,
                       issues: Boolean = true,
                       autoInit: Boolean = false): GHRepository =
    Try {
      getOrganization(owner).map(_.createRepository(repo)).getOrElse(gitHub.createRepository(repo))
        .description(description)
        .private_(privateFlag)
        .issues(issues)
        .autoInit(autoInit)
        .create()
    } match {
      case Success(repository) => repository
      case Failure(e) => throw ArtifactSourceCreationException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def createBranch(repo: String, owner: String, branchName: String, fromBranch: String): GHRef =
    getRepository(repo, owner) match {
      case Some(repository) => createBranch(repository, branchName, fromBranch)
      case None => throw ArtifactSourceUpdateException(s"Failed to find repository '$owner/$repo'")
    }

  @throws[ArtifactSourceUpdateException]
  def createBranch(repository: GHRepository, branchName: String, fromBranch: String): GHRef =
    Try {
      val ref = repository.getRef(s"heads/$fromBranch")
      repository.createRef(s"refs/heads/$branchName", ref.getObject.getSha)
    } match {
      case Success(reference) => reference
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def createBranchFromChanges(repo: String,
                              owner: String,
                              branchName: String,
                              fromBranch: String,
                              old: ArtifactSource,
                              current: ArtifactSource,
                              message: String): Seq[FileArtifact] =
    getRepository(repo, owner) match {
      case Some(repository) => createBranchFromChanges(repository, branchName, fromBranch, old, current, message)
      case None => throw ArtifactSourceUpdateException(s"Failed to find repository '$owner/$repo'")
    }

  @throws[ArtifactSourceUpdateException]
  def createBranchFromChanges(repository: GHRepository,
                              branchName: String,
                              fromBranch: String,
                              old: ArtifactSource,
                              current: ArtifactSource,
                              message: String): Seq[FileArtifact] =
    Try {
      if (branchName != fromBranch)
        createBranch(repository, branchName, fromBranch)

      val deltas = current.deltaFrom(old)
      val sui = GitHubSourceUpdateInfo(
        GitHubArtifactSourceLocator.fromStrings(repository.getName, repository.getOwnerName, branchName), message)
      val files = deltas.deltas.collect {
        case fad: FileAdditionDelta => fad.newFile
        case fud: FileUpdateDelta => fud.updatedFile
      }
      val filesToDelete = deltas.deltas.collect {
        case fdd: FileDeletionDelta => fdd.oldFile
      }
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
                                   message: String): Option[PullRequestStatus] =
    Try(getRepository(repo, owner).map(repository => {
      createBranchFromChanges(repository, prr.head, prr.base, old, current, message)
      Http(s"${getPath(repo, owner)}/pulls").postData(toJson(prr))
        .headers(headers)
        .execute(fromJson[PullRequestStatus])
        .throwError
        .body
    }).orNull) match {
      case Success(prs) => Option(prs)
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def createReviewComment(repo: String,
                          owner: String,
                          number: Int,
                          body: String,
                          commitId: String,
                          path: String,
                          position: Int): ReviewComment = {
    val crc = CreateReviewComment(body, commitId, path, position)
    Try {
      Http(s"${getPath(repo, owner)}/pulls/$number/comments").postData(toJson(crc))
        .headers(headers)
        .execute(fromJson[ReviewComment])
        .throwError
        .body
    } match {
      case Success(rc) => rc
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }
  }

  @throws[ArtifactSourceException]
  def commitFiles(sui: GitHubSourceUpdateInfo,
                  files: JList[FileArtifact],
                  filesToDelete: JList[FileArtifact]): JList[FileArtifact] =
    commitFiles(sui, files.asScala, filesToDelete.asScala).asJava

  @throws[ArtifactSourceException]
  def commitFiles(sui: GitHubSourceUpdateInfo,
                  files: Seq[FileArtifact],
                  filesToDelete: Seq[FileArtifact]): Seq[FileArtifact] = {
    val sourceId = sui.sourceId
    val repo = sourceId.repo
    val owner = sourceId.owner
    getRepository(repo, owner) match {
      case Some(repository) =>
        commitFiles(repository, sourceId.branch, sui.message, files, filesToDelete)
      case None =>
        logger.debug(s"Failed to find repository '$owner/$repo'")
        Nil
    }
  }

  @throws[ArtifactSourceException]
  def commitFiles(repository: GHRepository,
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

    val repo = repository.getName
    val owner = repository.getOwnerName
    Try {
      val gHBranch = repository.getBranch(branch)
      val baseTreeSha = gHBranch.getSHA1
      val fwbrs = files.map(fa => FileWithBlobRef(fa, createBlob(repo, owner, message, branch, fa)))
      val newOrUpdatedTreeEntries = fwbrs.map(fwbr => TreeEntry(fwbr.fa.path, intToOctal(fwbr.fa.mode), "blob", fwbr.ref.sha))
      val allExistingTreeEntries = treeFor(GitHubShaIdentifier(repo, owner, baseTreeSha)).allFiles
        .map(fa => TreeEntry(fa.path, intToOctal(fa.mode), "blob", fa.uniqueId.getOrElse("")))

      val treeEntriesToDelete = filesToDelete.map(fa => TreeEntry(fa.path, intToOctal(fa.mode), "blob", fa.uniqueId.getOrElse("")))

      val finalTreeEntries = (newOrUpdatedTreeEntries ++ allExistingTreeEntries)
        .groupBy(_.path)
        .map(_._2.head)
        .filterNot(te => treeEntriesToDelete.exists(_.path == te.path))
        .toSeq

      val tree = createTree(repo, owner, finalTreeEntries)
      logger.debug(tree.toString)
      val commit = createCommit(repo, owner, message, tree, Seq(baseTreeSha))
      updateReference(repo, owner, s"heads/$branch", commit.sha)

      fwbrs.map(fwbr => StringFileArtifact.withNewUniqueId(fwbr.fa, fwbr.ref.sha))
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to commit files to '$owner/$repo': ${e.getMessage}", e)
    }
  }

  @throws[ArtifactSourceUpdateException]
  def addFile(sui: GitHubSourceUpdateInfo, fa: FileArtifact): Option[FileArtifact] =
    Try {
      val sourceId = sui.sourceId
      val repo = sourceId.repo
      val owner = sourceId.owner
      getRepository(repo, owner) match {
        case Some(repository) =>
          val binaryContent = getBinaryContent(fa)
          val response = repository.createContent(binaryContent, sui.message, fa.path, sourceId.branch)
          Some(StringFileArtifact(fa.name, fa.pathElements, fa.content, fa.mode, Some(response.getContent.getSha)))
        case None =>
          logger.debug(s"Failed to find repository '$owner/$repo'")
          None
      }
    } match {
      case Success(fileArtifact) => fileArtifact
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def mergePullRequest(repo: String,
                       owner: String,
                       number: Int,
                       title: String,
                       message: String,
                       mergeMethod: String = "squash"): Option[PullRequestMerged] = {
    val prmr = PullRequestMergeRequest(title, message, mergeMethod)
    Try {
      Http(s"${getPath(repo, owner)}/pulls/$number/merge").postData(toJson(prmr))
        .method("PUT")
        .headers(headers)
        .execute(fromJson[PullRequestMerged])
        .throwError
        .body
    } match {
      case Success(prm) => Some(prm)
      case Failure(e) =>
        logger.debug(s"Failed to merge pull request $number: ${e.getMessage}")
        None
    }
  }

  private def getBinaryContent(fa: FileArtifact) =
    fa match {
      case b: ByteArrayFileArtifact => b.bytes
      case _ => fa.content.getBytes
    }

  private def createBlob(repo: String, owner: String, message: String, branch: String, fa: FileArtifact): GitHubRef = {
    val cbr = CreateBlobRequest(new String(Base64.encode(getBinaryContent(fa))))
    Http(s"${getPath(repo, owner)}/git/blobs").postData(toJson(cbr))
      .headers(headers)
      .execute(fromJson[GitHubRef])
      .throwError
      .body
  }

  private def createTree(repo: String, owner: String, treeEntries: Seq[TreeEntry]): CreateTreeResponse = {
    val ctr = CreateTreeRequest(treeEntries)
    logger.debug(ctr.toString)
    Http(s"${getPath(repo, owner)}/git/trees").postData(toJson(ctr))
      .headers(headers)
      .execute(fromJson[CreateTreeResponse])
      .throwError
      .body
  }

  private def createCommit(repo: String,
                           owner: String,
                           message: String,
                           tree: CreateTreeResponse,
                           parents: Seq[String]): CommitResponse = {
    val cr = CommitRequest(message, tree.sha, parents)
    Http(s"${getPath(repo, owner)}/git/commits").postData(toJson(cr))
      .headers(headers)
      .execute(fromJson[CommitResponse])
      .throwError
      .body
  }

  private def updateReference(repo: String, owner: String, ref: String, newSha: String): Unit = {
    val urr = UpdateReferenceRequest(newSha, force = true)
    Http(s"${getPath(repo, owner)}/git/refs/$ref").postData(toJson(urr))
      .method("PATCH")
      .headers(headers)
      .asBytes
      .throwError
  }

  private def getPath(repo: String, owner: String) =
    s"${gitHub.getApiUrl}/repos/$owner/$repo"
}

object GitHubServices {

  val DefaultApiUrl = "https://api.github.com"

  case class GitHubRef(url: String, sha: String)

  case class FileDeleteResponse(commit: CommitResponse)

  case class CommitResponse(sha: String, url: String, tree: GitHubRef, message: String, parents: Seq[GitHubRef])

  case class CreateTreeResponse(sha: String, url: String, tree: Seq[TreeElement])

  case class TreeElement(path: String, mode: String, `type`: String, size: Int, sha: String, url: String)

  case class PullRequestRequest(title: String, head: String, base: String, body: String)

  case class PullRequestStatus(id: Int,
                               url: String,
                               @JsonProperty("html_url") var htmlUrl: String,
                               number: Int,
                               title: String,
                               body: String,
                               @JsonProperty("created_at") createdAt: OffsetDateTime,
                               @JsonProperty("updated_at") updatedAt: OffsetDateTime,
                               @JsonProperty("merged_at") mergedAt: OffsetDateTime,
                               @JsonProperty("closed_at") closedAt: OffsetDateTime,
                               head: PullRequestBranch,
                               base: PullRequestBranch,
                               state: String,
                               merged: Boolean,
                               mergeable: Boolean,
                               @JsonProperty("mergeable_state") mergeableState: String,
                               comments: Int,
                               @JsonProperty("review_comments") reviewComments: Int) {

    import PullRequestStatus._

    def isOpen: Boolean = OpenState == state
  }

  object PullRequestStatus {

    val OpenState: String = "open"
    val ClosedState: String = "closed"
  }

  case class PullRequestBranch(ref: String, sha: String)

  case class PullRequestMerged(sha: String, merged: Boolean, message: String)

  case class ReviewComment(url: String,
                           id: Int,
                           @JsonProperty("pull_request_review_id") pullRequestReviewId: Int,
                           @JsonProperty("diff_hunk") diffHunk: String,
                           path: String,
                           position: Int,
                           @JsonProperty("original_position") originalPosition: Int,
                           @JsonProperty("commit_id") commitId: String,
                           @JsonProperty("original_commit_id") originalCommitId: String,
                           user: User,
                           body: String,
                           @JsonProperty("created_at") createdAt: OffsetDateTime,
                           @JsonProperty("updated_at") updatedAt: OffsetDateTime,
                           @JsonProperty("html_url") htmlUrl: String,
                           @JsonProperty("pull_request_url") pullRequestUrl: String,
                           @JsonProperty("_links") links: Links)

  case class User(login: String,
                  id: Int,
                  url: String,
                  @JsonProperty("html_url") htmlUrl: String,
                  @JsonProperty("type") `type`: String,
                  @JsonProperty("site_admin") siteAdmin: String)

  case class Links(self: LinksHref, html: LinksHref, @JsonProperty("pull_request") pullRequest: LinksHref)

  case class LinksHref(href: String)

  private case class FileWithBlobRef(fa: FileArtifact, ref: GitHubRef)

  private case class FileDeleteRequest(message: String, sha: String, branch: String)

  private case class CommitRequest(message: String, tree: String, parents: Seq[String])

  private case class CreateBlobRequest(content: String, encoding: String = "base64")

  private case class CreateTreeRequest(tree: Seq[TreeEntry])

  private case class TreeEntry(path: String, mode: String, `type`: String, sha: String)

  private case class UpdateReferenceRequest(sha: String, force: Boolean)

  private case class CreateReviewComment(body: String,
                                         @JsonProperty("commit_id") commitId: String,
                                         path: String,
                                         position: Int)

  private case class PullRequestMergeRequest(@JsonProperty("commit_title") title: String,
                                             @JsonProperty("commit_message") message: String,
                                             @JsonProperty("merge_method") mergeMethod: String)
}
