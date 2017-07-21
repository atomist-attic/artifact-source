package com.atomist.source.git.github

import java.io.InputStream
import java.nio.charset.Charset

import com.atomist.source.git.github.domain._
import com.atomist.source.{ArtifactSourceUpdateException, FileArtifact, _}
import com.atomist.util.JsonUtils.{fromJson, toJson}
import com.atomist.util.Octal.intToOctal
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import resource._

import scala.util.{Failure, Success, Try}
import scalaj.http.{Base64, Http, HttpResponse}

case class GitHubServices(oAuthToken: String, apiUrl: Option[String] = None)
  extends GitHubSourceReader
    with LazyLogging {

  import GitHubServices._

  private val headers: Map[String, String] =
    Map(
      "Authorization" -> ("token " + oAuthToken),
      "Accept" -> "application/vnd.github.v3+json,application/vnd.github.loki-preview+json"
    )

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = TreeGitHubArtifactSource(id, this)

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = TreeGitHubArtifactSource(id, this)

  def getOrganization(owner: String): Option[Organization] =
    Try(Http(s"$ApiUrl/orgs/$owner")
      .headers(headers)
      .execute(fromJson[Organization])
      .throwError
      .body) match {
      case Success(org) => Some(org)
      case Failure(e) =>
        logger.warn(s"Failed to find organization $owner", e)
        None
    }

  def getRepository(repo: String, owner: String): Option[Repository] =
    Try(Http(s"$ApiUrl/repos/$owner/$repo")
      .headers(headers)
      .execute(fromJson[Repository])
      .throwError
      .body) match {
      case Success(r) => Some(r)
      case Failure(e) =>
        logger.warn(s"Failed to find user repository $owner/$repo. Searching for organization repository", e)
        val params = Map("per_page" -> "100")
        val resp = Http(s"$ApiUrl/orgs/$owner/repos").headers(headers).params(params).asString
        if (resp.isSuccess) {
          val firstPage = fromJson[Seq[Repository]](resp.body)
          getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage).find(_.name == repo)
        } else {
          logger.warn(s"Failed to find user or organization repository $owner/$repo: ${resp.body}")
          None
        }
    }

  def searchRepositories(q: String): Seq[Repository] = {
    val params = Map("q" -> q, "per_page" -> "100")

    def nextPage(url: String, accumulator: Seq[Repository]): Seq[Repository] = {
      val resp = Http(url).headers(headers).params(params).asString
      if (resp.isSuccess) {
        val pages = accumulator ++ fromJson[SearchRepoResult](resp.body).items
        getNextUrl(resp).map(nextPage(_, pages)).getOrElse(pages)
      } else {
        logger.warn(s"Failed to find repositories: ${resp.body}")
        Seq.empty
      }
    }

    val resp = Http(s"$ApiUrl/search/repositories").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[SearchRepoResult](resp.body).items
      getNextUrl(resp).map(nextPage(_, firstPage)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to find repositories: ${resp.body}")
      Seq.empty
    }
  }

  @throws[ArtifactSourceCreationException]
  def createRepository(repo: String,
                       owner: String,
                       description: String = "",
                       privateFlag: Boolean = false,
                       issues: Boolean = true,
                       autoInit: Boolean = false): Repository =
    Try {
      val url = getOrganization(owner).map(_ => s"$ApiUrl/orgs/$owner/repos").getOrElse(s"$ApiUrl/user/repos")
      val cr = CreateRepoRequest(repo, owner, description, privateFlag, issues, autoInit)
      Http(url).postData(toJson(cr))
        .headers(headers)
        .execute(fromJson[Repository])
        .throwError
        .body
    } match {
      case Success(repository) => repository
      case Failure(e) => throw ArtifactSourceCreationException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def deleteRepository(repo: String, owner: String): Unit =
    Try(Http(s"$ApiUrl/repos/$owner/$repo")
      .method("DELETE")
      .headers(headers)
      .asString
      .throwError
      .body
    ) match {
      case Success(_) => logger.debug(s"Successfully deleted repository $owner/$repo")
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to delete repository $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def createBranch(repo: String, owner: String, branchName: String, fromBranch: String): Reference =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/git/refs/heads/$fromBranch")
      .headers(headers)
      .execute(fromJson[Reference])
      .throwError
      .body) match {
      case Success(fromRef) =>
        val cr = CreateReferenceRequest(s"refs/heads/$branchName", fromRef.`object`.sha)
        val resp = Http(s"$ApiUrl/repos/$owner/$repo/git/refs").headers(headers).postData(toJson(cr)).asString
        if (resp.isSuccess)
          fromJson[Reference](resp.body)
        else
          throw ArtifactSourceUpdateException(s"Failed to create branch $branchName in $owner/$repo: ${resp.body}")
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
    Try {
      getBranch(repo, owner, branchName).getOrElse(createBranch(repo, owner, branchName, fromBranch))

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
                                   message: String): PullRequest = {
    createBranchFromChanges(repo, owner, prr.head, prr.base, old, current, message)
    createPullRequest(repo, owner, prr)
  }

  @throws[ArtifactSourceUpdateException]
  def createPullRequest(repo: String,
                        owner: String,
                        prr: PullRequestRequest): PullRequest =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls").postData(toJson(prr))
      .headers(headers)
      .execute(fromJson[PullRequest])
      .throwError
      .body) match {
      case Success(pr) => pr
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to create pull request for $owner/$repo", e)
    }

  def getBranch(repo: String, owner: String, branch: String): Option[Branch] =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/branches/$branch")
      .headers(headers)
      .execute(fromJson[Branch])
      .throwError
      .body) match {
      case Success(b) => Some(b)
      case Failure(e) =>
        logger.debug(s"Failed to find branch $branch in $owner/$repo", e)
        None
    }

  @throws[ArtifactSourceUpdateException]
  def createReviewComment(repo: String,
                          owner: String,
                          number: Int,
                          body: String,
                          commitId: String,
                          path: String,
                          position: Int): ReviewComment =
    Try {
      val crc = CreateReviewCommentRequest(body, commitId, path, position)
      logger.debug(crc.toString)
      Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number/comments").postData(toJson(crc))
        .headers(headers)
        .execute(fromJson[ReviewComment])
        .throwError
        .body
    } match {
      case Success(rc) => rc
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create review comment for pull request $number in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def commitFiles(sui: GitHubSourceUpdateInfo,
                  files: Seq[FileArtifact],
                  filesToDelete: Seq[FileArtifact]): Seq[FileArtifact] = {
    val id = sui.sourceId
    commitFiles(id.repo, id.owner, id.branch, sui.message, files, filesToDelete)
  }

  /**
    * Commits files to a branch.
    *
    * This process is involved, so here's a summary of what has to be done:
    * 1. Get a reference to the desired branch's HEAD
    * 2. Get the commit from the branch. We need the SHA.
    * 3. Post the new and updated files to the server as blobs, saving their SHAs
    * 4. Get the tree, recursively, that the commit points to from (2)
    * 5. Create a new tree containing the new and updated files, as well as omitting files to be deleted
    * 6. Create a new commit, referring to the tree from (5).
    * 7. Update our new branch's heads/master reference to point to our new commit from (6).
    */
  @throws[ArtifactSourceUpdateException]
  def commitFiles(repo: String,
                  owner: String,
                  branch: String,
                  message: String,
                  files: Seq[FileArtifact],
                  filesToDelete: Seq[FileArtifact]): Seq[FileArtifact] = {
    val baseTreeSha = getBranch(repo, owner, branch) match {
      case Some(br) => br.commit.sha
      case None => throw ArtifactSourceUpdateException(s"Failed to get branch $branch in $owner/$repo")
    }

    Try {
      val filesWithBlobRefs = files
        .groupBy(_.path)
        .map(_._2.last)
        .map(fa => FileWithBlobRef(fa, createBlob(repo, owner, message, branch, fa)))
        .toSeq

      val newOrUpdatedTreeEntries = filesWithBlobRefs
        .map(fileWithBlobRef => TreeEntry(fileWithBlobRef.fa.path, intToOctal(fileWithBlobRef.fa.mode), "blob", fileWithBlobRef.ref.sha))

      val allExistingTreeEntries = treeFor(GitHubShaIdentifier(repo, owner, baseTreeSha))
        .allFiles.map(fa => TreeEntry(fa.path, intToOctal(fa.mode), "blob", fa.uniqueId.getOrElse("")))

      val treeEntriesToDelete = filesToDelete.map(fa => TreeEntry(fa.path, intToOctal(fa.mode), "blob", fa.uniqueId.getOrElse("")))

      val finalTreeEntries = (allExistingTreeEntries ++ newOrUpdatedTreeEntries)
        .groupBy(_.path)
        .map(_._2.last)
        .filterNot(te => treeEntriesToDelete.exists(_.path == te.path))
        .toSeq

      val tree = createTree(repo, owner, finalTreeEntries)
      logger.debug(tree.toString)

      val commit = createCommit(repo, owner, message, tree, Seq(baseTreeSha))
      updateReference(repo, owner, s"heads/$branch", commit.sha)

      filesWithBlobRefs.map(fileWithBlobRef => fileWithBlobRef.fa.withUniqueId(fileWithBlobRef.ref.sha))
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to commit files to $owner/$repo", e)
    }
  }

  @throws[ArtifactSourceUpdateException]
  def addFile(sui: GitHubSourceUpdateInfo, fa: FileArtifact): FileArtifact = {
    val sourceId = sui.sourceId
    addFile(sourceId.repo, sourceId.owner, sourceId.branch, sui.message, fa)
  }

  @throws[ArtifactSourceUpdateException]
  def addFile(repo: String,
              owner: String,
              branch: String,
              message: String,
              fa: FileArtifact): FileArtifact =
    Try {
      val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
      val cf = CreateFileRequest(fa.path, message, content, branch)
      Http(s"$ApiUrl/repos/$owner/$repo/contents/${fa.path}").postData(toJson(cf))
        .method("PUT")
        .headers(headers)
        .execute(fromJson[CreateFileResponse])
        .throwError
        .body
    } match {
      case Success(cfr) => fa.withUniqueId(cfr.content.sha)
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to add file to $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def mergePullRequest(repo: String,
                       owner: String,
                       number: Int,
                       title: String,
                       message: String,
                       mergeMethod: String = "squash"): PullRequestMerged =
    Try {
      val prmr = PullRequestMergeRequest(title, message, mergeMethod)
      logger.debug(prmr.toString)
      Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number/merge").postData(toJson(prmr))
        .method("PUT")
        .headers(headers)
        .execute(fromJson[PullRequestMerged])
        .throwError
        .body
    } match {
      case Success(prm) => prm
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to merge pull request $number in $owner/$repo", e)
    }

  def getCommits(repo: String, owner: String, sha: Option[String] = None): Seq[Commit] = {
    val params = sha.map(s => Map("sha" -> s)).getOrElse(Map.empty) + ("per_page" -> "100")
    val resp = Http(s"$ApiUrl/repos/$owner/$repo/commits").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[Seq[Commit]](resp.body)
      getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to get commits in $owner/$repo: ${resp.body}")
      Seq.empty
    }
  }

  def getPullRequest(repo: String, owner: String, number: Int): Option[PullRequest] =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/pulls/$number")
      .headers(headers)
      .execute(fromJson[PullRequest])
      .throwError
      .body) match {
      case Success(pr) => Some(pr)
      case Failure(e) =>
        logger.warn(s"Failed to get pull request $number in $owner/$repo", e)
        None
    }

  def getPullRequests(repo: String,
                      owner: String,
                      state: String = PullRequest.Open,
                      sort: String = "created",
                      direction: String = "asc"): Seq[PullRequest] = {
    val params = Map("state" -> state, "sort" -> sort, "direction" -> direction, "per_page" -> "100")
    val resp = Http(s"$ApiUrl/repos/$owner/$repo/pulls").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[Seq[PullRequest]](resp.body)
      getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to get pull requests in $owner/$repo: ${resp.body}")
      Seq.empty
    }
  }

  @throws[ArtifactSourceAccessException]
  def getTreeRecursive(repo: String, owner: String, sha: String): Tree =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/git/trees/$sha")
      .param("recursive", "1")
      .headers(headers)
      .execute(fromJson[Tree])
      .throwError
      .body) match {
      case Success(tree) => tree
      case Failure(e) => throw ArtifactSourceAccessException(s"Failed to get tree with sha $sha in $owner/$repo", e)
    }

  @throws[ArtifactSourceAccessException]
  def readBlob(repo: String, owner: String, sha: String): InputStream =
    Try(Http(s"$ApiUrl/repos/$owner/$repo/git/blobs/$sha")
      .timeout(connTimeoutMs = 2000, readTimeoutMs = 10000)
      .header("Authorization", "token " + oAuthToken)
      .header("Accept", "application/vnd.github.v3.raw+json")
      .execute(IOUtils.toBufferedInputStream)
      .throwError
      .body) match {
      case Success(is) => is
      case Failure(e) =>
        throw ArtifactSourceAccessException(s"Failed to read blob with sha $sha in $owner/$repo", e)
    }

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

  private def createTree(repo: String, owner: String, treeEntries: Seq[TreeEntry]): Tree = {
    val ctr = CreateTreeRequest(treeEntries)
    logger.debug(ctr.toString)
    Http(s"$ApiUrl/repos/$owner/$repo/git/trees").postData(toJson(ctr))
      .headers(headers)
      .exec((code: Int, headers: Map[String, IndexedSeq[String]], is: InputStream) => code match {
        case 201 => fromJson[Tree](is)
        case _ =>
          throw new IllegalArgumentException(s"${headers("Status").head}, ${IOUtils.toString(is, Charset.defaultCharset)}")
      }).body
  }

  private def createCommit(repo: String,
                           owner: String,
                           message: String,
                           tree: Tree,
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

  private def paginateResults[T](url: String,
                                 firstPage: Seq[T],
                                 params: Map[String, String] = Map.empty)(implicit m: Manifest[T]): Seq[T] = {
    def nextPage(url: String, accumulator: Seq[T]): Seq[T] = {
      val resp = Http(url).headers(headers).params(params).asString
      if (resp.isSuccess) {
        val pages = accumulator ++ fromJson[Seq[T]](resp.body)
        getNextUrl(resp).map(nextPage(_, pages)).getOrElse(pages)
      } else
        throw ArtifactSourceAccessException(resp.body)
    }

    nextPage(url, firstPage)
  }

  private def getNextUrl(resp: HttpResponse[String]): Option[String] =
    resp.header("Link").flatMap(parseLinkHeader(_).get("next"))
}

object GitHubServices {

  val Url = "https://github.com"
  val ApiUrl = "https://api.github.com"

  private def parseLinkHeader(linkHeader: String): Map[String, String] =
    if (linkHeader == null || linkHeader.isEmpty) Map.empty
    else
      linkHeader.split(',').map { part =>
        val section = part.split(';')
        val url = section(0).replace("<", "").replace(">", "").trim
        val name = section(1).replace(" rel=\"", "").replace("\"", "").trim
        (name, url)
      }.toMap
}
