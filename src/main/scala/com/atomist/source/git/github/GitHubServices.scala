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
import org.kohsuke.github.{GHRepository, GitHub, _}
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
    Map("Authorization" -> ("token " + oAuthToken),
      "Accept" -> "application/vnd.github.v3+json, application/vnd.github.loki-preview+json")

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = TreeGitHubArtifactSource(id, this)

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = TreeGitHubArtifactSource(id, this)

  def getOrganization(owner: String): Option[GHOrganization] = {
    require(Option(owner).exists(_.trim.nonEmpty), "owner must not be null or empty")

    Try(gitHub.getOrganization(owner)) match {
      case Success(organization) => Some(organization)
      case Failure(e) =>
        logger.warn(e.getMessage, e)
        None
    }
  }

  def getRepository1(repo: String, owner: String): Option[Repo] = {
    Try(Http(s"$ApiUrl/repos/$owner/$repo")
      .headers(headers)
      .execute(fromJson[Repo])
      .throwError
      .body) match {
      case Success(r) => Some(r)
      case Failure(e) =>
        logger.warn(e.getMessage, e)
        Http(s"$ApiUrl/orgs/$owner/repos")
          .headers(headers)
          .execute(fromJson[Seq[Repo]])
          .throwError
          .body
          .find(_.name == repo)
    }
  }

  def getRepository(repo: String, owner: String): Option[GHRepository] = {
    require(Option(repo).exists(_.trim.nonEmpty), "repo must not be null or empty")
    require(Option(owner).exists(_.trim.nonEmpty), "owner must not be null or empty")
    Try(gitHub.getRepository(s"$owner/$repo")) match {
      case Success(repository) => Option(repository)
      case Failure(e) =>
        logger.warn(e.getMessage, e)
        Option(getOrganization(owner).map(_.getRepository(repo)).orNull)
    }
  }

  @throws[ArtifactSourceCreationException]
  def createRepository(repo: String,
                       owner: String,
                       description: String = "",
                       privateFlag: Boolean = false,
                       issues: Boolean = true,
                       autoInit: Boolean = false): GHRepository = {
    val cr = CreateRepo(repo, owner, description, privateFlag, issues, autoInit)
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
  }

  def createBranch(repo: String, owner: String, branchName: String, fromBranch: String): Reference = {
    val fromRef = Http(s"$ApiUrl/repos/$owner/$repo/git/refs/heads/$fromBranch")
      .headers(headers)
      .execute(fromJson[Reference])
      .throwError
      .body
    val cr = CreateReference(s"refs/heads/$branchName", fromRef.`object`.sha)
    Http(s"$ApiUrl/repos/$owner/$repo/git/refs").postData(toJson(cr))
      .headers(headers)
      .execute(fromJson[Reference])
      .throwError
      .body
  }

  //  @throws[ArtifactSourceUpdateException]
  //  def createBranch1(repo: String, owner: String, branchName: String, fromBranch: String): GHRef =
  //    getRepository(repo, owner) match {
  //      case Some(repository) => createBranch(repository, branchName, fromBranch)
  //      case None => throw ArtifactSourceUpdateException(s"Failed to find repository '$owner/$repo'")
  //    }

  //  @throws[ArtifactSourceUpdateException]
  //  def createBranch(repository: GHRepository, branchName: String, fromBranch: String): GHRef =
  //    Try {
  //      val ref = repository.getRef(s"heads/$fromBranch")
  //      repository.createRef(s"refs/heads/$branchName", ref.getObject.getSha)
  //    } match {
  //      case Success(reference) => reference
  //      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
  //    }

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
    Option(Http(s"$ApiUrl/repos/$owner/$repo/pulls").postData(toJson(prr))
      .headers(headers)
      .execute(fromJson[PullRequestStatus])
      .throwError
      .body)
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
    Try {
      Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number/comments").postData(toJson(crc))
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
    Try {
      val cfr = Http(s"$ApiUrl/repos/$owner/$repo/contents/${fa.path}").postData(toJson(cf))
        .method("PUT")
        .headers(headers)
        .execute(fromJson[CreateFileResponse])
        .throwError
        .body
      Some(fa.withUniqueId(cfr.content.sha))
    } match {
      case Success(fileArtifact) => fileArtifact
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to add file to '$owner/$repo': ${e.getMessage}", e)
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
    Try {
      Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number/merge").postData(toJson(prmr))
        .method("PUT")
        .headers(headers)
        .execute(fromJson[PullRequestMerged])
        .throwError
        .body
    } match {
      case Success(prm) => Some(prm)
      case Failure(e) =>
        logger.warn(s"Failed to merge pull request $number: ${e.getMessage}", e)
        None
    }
  }

  def getCommits(repo: String, owner: String): Seq[String] =
    Http(s"$ApiUrl/repos/$owner/$repo/git/refs")
      .headers(headers)
      .execute(fromJson[Seq[Reference]])
      .throwError
      .body
      .filter(_.`object`.`type` == "commit")
      .map(_.`object`.sha)

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
                           parents: Seq[String]): CommitResponse = {
    val cr = CommitRequest(message, tree.sha, parents)
    logger.debug(cr.toString)
    Http(s"$ApiUrl/repos/$owner/$repo/git/commits").postData(toJson(cr))
      .headers(headers)
      .execute(fromJson[CommitResponse])
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