package com.atomist.source.git

import java.io.InputStream
import java.nio.charset.Charset.defaultCharset
import java.util.{List => JList}

import com.atomist.source.git.Retry.retry
import com.atomist.source.git.domain.ReactionContent.ReactionContent
import com.atomist.source.git.domain._
import com.atomist.source.{ArtifactSourceException, FileArtifact, _}
import com.atomist.util.HttpMethod._
import com.atomist.util.JsonUtils.{fromJson, toJson}
import com.atomist.util.Octal.intToOctal
import com.atomist.util.RestGateway
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.http.client.utils.URLEncodedUtils
import resource._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scalaj.http.{Base64, Http, HttpResponse}

case class GitHubServices(oAuthToken: String, apiUrl: Option[String] = None)
  extends RestGateway
    with GitHubSourceReader
    with LazyLogging {

  def this(oAuthToken: String, apiUrl: String) = this(oAuthToken, Option(apiUrl)) // For Java

  import GitHubServices._

  private val api: String = apiUrl match {
    case Some(url) => url match {
      case "" => ApiUrl
      case u if u.endsWith("/") => u.dropRight(1)
      case _ => url
    }
    case None => ApiUrl
  }

  private val headers: Map[String, String] =
    Map(
      "Authorization" -> ("token " + oAuthToken),
      "Accept" -> Seq(
        "application/vnd.github.v3+json",
        "application/vnd.github.loki-preview",
        "application/vnd.github.squirrel-girl-preview",
        "application/vnd.github.cloak-preview"
      ).mkString(",")
    )

  override def httpRequest[T](url: String,
                              method: HttpMethod = Get,
                              body: Option[Array[Byte]] = None,
                              params: Map[String, String] = Map.empty,
                              headers: Map[String, String] = headers)(implicit m: Manifest[T]): T =
    retry(url) {
      (body match {
        case Some(data) => method match {
          case Post => Http(url).postData(data)
          case Put => Http(url).put(data)
          case Patch => Http(url).postData(data).method(method.toString)
          case _ => Http(url).postData(data)
        }
        case None => Http(url).method(method.toString)
      }).headers(headers)
        .params(params)
        .exec((code: Int, headers: Map[String, IndexedSeq[String]], is: InputStream) => code match {
          case 200 | 201 => fromJson[T](is)
          case success if 202 until 206 contains success => ().asInstanceOf[T]
          case 304 => ().asInstanceOf[T]
          case 401 | 403 | 415 | 422 =>
            val msg = IOUtils.toString(is, defaultCharset)
            logger.debug(s"${headers("Status").head}, $msg")
            throw DoNotRetryException(msg)
          case _ =>
            val msg = IOUtils.toString(is, defaultCharset)
            logger.debug(s"${headers("Status").head}, $msg")
            throw ArtifactSourceException(msg)
        }).body
    }

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = TreeGitHubArtifactSource(id, this)

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = TreeGitHubArtifactSource(id, this)

  def hasRepository(repo: String, owner: String): Boolean =
    Try(searchRepositories(Map("q" -> s"repo:$owner/$repo"))) match {
      case Success(result) => result.items.size == 1
      case Failure(e) =>
        logger.debug(e.getMessage)
        false
    }

  def searchRepositories(params: Map[String, String] = Map("per_page" -> "100")): SearchResult[Repository] =
    paginateSearchResults[Repository](s"$api/search/repositories", params)

  @throws[ArtifactSourceException]
  def createRepository(repo: String,
                       owner: String,
                       description: String = "",
                       privateFlag: Boolean = false,
                       issues: Boolean = true,
                       autoInit: Boolean = false): Repository = {
    val data = toJson(Map("name" -> repo,
      "description" -> description,
      "private" -> privateFlag,
      "has_issues" -> issues,
      "auto_init" -> autoInit))

    Try(httpRequest[Repository](s"$api/orgs/$owner/repos", Post, Some(data))) match {
      case Success(repository) => repository
      case Failure(e) =>
        logger.debug(s"Failed to create organization repository: ${e.getMessage}. Attempting to create user repository ...")
        Try(httpRequest[Repository](s"$api/user/repos", Post, Some(data))) match {
          case Success(repository) => repository
          case Failure(ex) => throw ArtifactSourceException(s"Failed to create $owner/$repo", ex)
        }
    }
  }

  @throws[ArtifactSourceException]
  def deleteRepository(repo: String, owner: String): Unit =
    httpRequest[Unit](s"$api/repos/$owner/$repo", Delete)

  def getBranch(repo: String, owner: String, branch: String): Option[Branch] =
    Try(httpRequest[Branch](s"$api/repos/$owner/$repo/branches/$branch")) match {
      case Success(br) => Some(br)
      case Failure(e) =>
        logger.debug(e.getMessage)
        None
    }

  def listBranches(repo: String, owner: String): Seq[Branch] =
    Try(paginateResults[Branch](s"$api/repos/$owner/$repo/branches")) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }

  @throws[ArtifactSourceException]
  def createBranch(repo: String, owner: String, branchName: String, fromBranch: String): Reference =
    Try(httpRequest[Reference](s"$api/repos/$owner/$repo/git/refs/heads/$fromBranch")) match {
      case Success(fromRef) => createReference(repo, owner, branchName, fromRef.`object`.sha)
      case Failure(e) => throw e
    }

  @throws[ArtifactSourceException]
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

      val filesToAdd = deltas.deltas.collect {
        case fad: FileAdditionDelta if !filesToUpdate.exists(_.path == fad.path) => fad.newFile
      }

      val files = filesToUpdate ++ filesToAdd

      val filesToDelete = deltas.deltas.collect {
        case fdd: FileDeletionDelta => fdd.oldFile
      }

      commitFiles(sui, files, filesToDelete)
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) => throw ArtifactSourceException(e.getMessage, e)
    }

  @throws[ArtifactSourceException]
  def deleteBranch(repo: String, owner: String, branchName: String): Unit =
    httpRequest[Unit](s"$api/repos/$owner/$repo/git/refs/heads/$branchName", Delete)

  @throws[ArtifactSourceException]
  def createReference(repo: String, owner: String, ref: String, sha: String): Reference = {
    val data = toJson(Map("ref" -> s"refs/heads/$ref", "sha" -> sha))
    httpRequest[Reference](s"$api/repos/$owner/$repo/git/refs", Post, Some(data))
  }

  @throws[ArtifactSourceException]
  def createPullRequestFromChanges(repo: String,
                                   owner: String,
                                   prr: PullRequestRequest,
                                   old: ArtifactSource,
                                   current: ArtifactSource,
                                   message: String): PullRequest = {
    createBranchFromChanges(repo, owner, prr.head, prr.base, old, current, message)
    createPullRequest(repo, owner, prr)
  }

  @throws[ArtifactSourceException]
  def createPullRequest(repo: String,
                        owner: String,
                        prr: PullRequestRequest): PullRequest =
    httpRequest[PullRequest](s"$api/repos/$owner/$repo/pulls", Post, Some(toJson(prr)))

  @throws[ArtifactSourceException]
  def mergePullRequest(repo: String,
                       owner: String,
                       number: Int,
                       title: String,
                       message: String,
                       mergeMethod: String = "squash"): PullRequestMerged = {
    val data = toJson(Map("commit_title" -> title, "commit_message" -> message, "merge_method" -> mergeMethod))
    httpRequest[PullRequestMerged](s"$api/repos/$owner/$repo/pulls/$number/merge", Put, Some(data))
  }

  def getPullRequest(repo: String, owner: String, number: Int): Option[PullRequest] =
    Try(httpRequest[PullRequest](s"$api/repos/$owner/$repo/pulls/$number")) match {
      case Success(pr) => Some(pr)
      case Failure(e) =>
        logger.debug(e.getMessage)
        None
    }

  def listPullRequests(repo: String,
                       owner: String,
                       state: String = PullRequest.Open,
                       sort: String = "created",
                       direction: String = "asc"): Seq[PullRequest] = {
    val params = Map("state" -> state, "sort" -> sort, "direction" -> direction, "per_page" -> "100")
    Try(paginateResults[PullRequest](s"$api/repos/$owner/$repo/pulls", params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }
  }

  @throws[ArtifactSourceException]
  def createPullRequestReviewComment(repo: String,
                                     owner: String,
                                     number: Int,
                                     body: String,
                                     commitId: String,
                                     path: String,
                                     position: Int): ReviewComment = {
    val data = toJson(Map("body" -> body, "commit_id" -> commitId, "path" -> path, "position" -> position))
    httpRequest[ReviewComment](s"$api/repos/$owner/$repo/pulls/$number/comments", Post, Some(data))
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
  @throws[ArtifactSourceException]
  def commitFiles(repo: String,
                  owner: String,
                  branch: String,
                  message: String,
                  files: Seq[FileArtifact],
                  filesToDelete: Seq[FileArtifact]): Seq[FileArtifact] = {
    val baseTreeSha = getBranch(repo, owner, branch) match {
      case Some(br) => br.commit.sha
      case None => throw ArtifactSourceException(s"Failed to get branch $branch in $owner/$repo")
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

      val commit = createCommit(repo, owner, message, tree.sha, Seq(baseTreeSha))
      updateReference(repo, owner, s"heads/$branch", commit.sha)

      filesWithBlobRefs.map(fileWithBlobRef => fileWithBlobRef.fa.withUniqueId(fileWithBlobRef.ref.sha))
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) => throw ArtifactSourceException(s"Failed to commit files to $owner/$repo", e)
    }
  }

  @throws[ArtifactSourceException]
  def addOrUpdateFile(sui: GitHubSourceUpdateInfo, fa: FileArtifact): FileArtifact = {
    val sourceId = sui.sourceId
    addOrUpdateFile(sourceId.repo, sourceId.owner, sourceId.branch, sui.message, fa)
  }

  @throws[ArtifactSourceException]
  def addOrUpdateFile(repo: String,
                      owner: String,
                      branch: String,
                      message: String,
                      fa: FileArtifact): FileArtifact = {
    val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
    val data = toJson(Map("path" -> fa.path, "message" -> message, "content" -> content, "branch" -> branch) ++
      fa.uniqueId.map(sha => Seq("sha" -> sha)).getOrElse(Nil))

    Try(httpRequest[CreateOrUpdateFileResponse](s"$api/repos/$owner/$repo/contents/${fa.path}", Put, Some(data))) match {
      case Success(cfr) => fa.withUniqueId(cfr.commit.sha)
      case Failure(e) => throw e
    }
  }

  def getFileContents(repo: String, owner: String, path: String): Seq[Content] =
    Try(httpRequest[Seq[Content]](s"$api/repos/$owner/$repo/contents/$path")) match {
      case Success(contents) => contents
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }

  def getCommit(repo: String, owner: String, sha: String): Option[Commit] =
    Try(httpRequest[Commit](s"$api/repos/$owner/$repo/git/commits/$sha")) match {
      case Success(commit) => Some(commit)
      case Failure(e) =>
        logger.debug(e.getMessage)
        None
    }

  def listCommits(repo: String, owner: String, sha: Option[String] = None): Seq[Commit] = {
    val params = sha.map(s => Map("sha" -> s)).getOrElse(Map.empty) + ("per_page" -> "100")
    Try(paginateResults[Commit](s"$api/repos/$owner/$repo/commits", params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }
  }

  def searchCommits(params: Map[String, String] = Map("per_page" -> "100")): SearchResult[Commit] =
    paginateSearchResults[Commit](s"$api/search/commits", params)

  @throws[ArtifactSourceException]
  def createCommitComment(repo: String,
                          owner: String,
                          sha: String,
                          body: String,
                          path: String,
                          position: Int): CommitComment = {
    val data = toJson(Map("body" -> body, "path" -> path, "position" -> position))
    httpRequest[CommitComment](s"$api/repos/$owner/$repo/commits/$sha/comments", Post, Some(data))
  }

  @throws[ArtifactSourceException]
  def getTreeRecursive(repo: String, owner: String, sha: String): Tree =
    httpRequest[Tree](s"$api/repos/$owner/$repo/git/trees/$sha", params = Map("recursive" -> "1"))

  @throws[ArtifactSourceException]
  def readBlob(repo: String, owner: String, sha: String): InputStream =
    retry("readBlob") {
      Try(Http(s"$api/repos/$owner/$repo/git/blobs/$sha")
        .timeout(connTimeoutMs = 2000, readTimeoutMs = 20000)
        .header("Authorization", "token " + oAuthToken)
        .header("Accept", "application/vnd.github.v3.raw+json")
        .execute(IOUtils.toBufferedInputStream)
        .throwError
        .body) match {
        case Success(is) => is
        case Failure(e) => throw ArtifactSourceException(s"Failed to read blob with sha $sha in $owner/$repo", e)
      }
    }

  @throws[ArtifactSourceException]
  def createWebhook(repo: String,
                    owner: String,
                    name: String,
                    url: String,
                    contentType: String,
                    active: Boolean,
                    events: Array[String]): Webhook =
    createWebhook(s"$api/repos/$owner/$repo/hooks", name, url, contentType, active, events)

  @throws[ArtifactSourceException]
  def createOrganizationWebhook(owner: String,
                                name: String,
                                url: String,
                                contentType: String,
                                active: Boolean,
                                events: Array[String]): Webhook =
    createWebhook(s"$api/orgs/$owner/hooks", name, url, contentType, active, events)

  def testWebhook(repo: String, owner: String, id: Int): Unit =
    Try(httpRequest[Unit](s"$api/repos/$owner/$repo/hooks/$id/tests", Post, Some("".getBytes))) match {
      case Success(_) =>
      case Failure(e) => logger.debug(e.getMessage)
    }

  @throws[ArtifactSourceException]
  def deleteWebhook(repo: String, owner: String, id: Int): Unit =
    httpRequest[Unit](s"$api/repos/$owner/$repo/hooks/$id", Delete)

  @throws[ArtifactSourceException]
  def deleteOrganizationWebhook(owner: String, id: Int): Unit =
    httpRequest[Unit](s"$api/orgs/$owner/hooks/$id", Delete)

  def listWebhooks(repo: String, owner: String, params: Map[String, String] = Map("per_page" -> "100")): Seq[Webhook] =
    Try(paginateResults[Webhook](s"$api/repos/$owner/$repo/hooks", params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }

  def listOrganizationWebhooks(owner: String, params: Map[String, String] = Map("per_page" -> "100")): Seq[Webhook] =
    Try(paginateResults[Webhook](s"$api/orgs/$owner/hooks", params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }

  @throws[ArtifactSourceException]
  def addCollaborator(repo: String, owner: String, collaborator: String): Unit =
    httpRequest[Unit](s"$api/repos/$owner/$repo/collaborators/$collaborator", Put, params = Map("permission" -> "push"))

  def getIssue(repo: String, owner: String, number: Int): Option[Issue] =
    Try(httpRequest[Issue](s"$api/repos/$owner/$repo/issues/$number")) match {
      case Success(issue) => Some(issue)
      case Failure(e) =>
        logger.debug(e.getMessage)
        None
    }

  def listIssues(params: Map[String, String] = Map("per_page" -> "100")): Seq[Issue] =
    Try(paginateResults[Issue](s"$api/issues", params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }

  def searchIssues(params: Map[String, String] = Map("per_page" -> "100")): SearchResult[Issue] =
    paginateSearchResults[Issue](s"$api/search/issues", params)

  @throws[ArtifactSourceException]
  def createIssue(repo: String,
                  owner: String,
                  title: String,
                  body: String,
                  labels: Seq[String] = Seq.empty,
                  assignees: Seq[String] = Seq.empty): Issue = {
    val data = toJson(Map("title" -> title, "body" -> body, "labels" -> labels, "assignees" -> assignees))
    httpRequest[Issue](s"$api/repos/$owner/$repo/issues", Post, Some(data))
  }

  @throws[ArtifactSourceException]
  def editIssue(repo: String,
                owner: String,
                number: Int,
                title: String,
                body: String,
                state: String = "open",
                labels: Seq[String] = Seq.empty,
                assignees: Seq[String] = Seq.empty): Issue = {
    val data = toJson(Map("title" -> title, "body" -> body, "state" -> state, "labels" -> labels, "assignees" -> assignees))
    httpRequest[Issue](s"$api/repos/$owner/$repo/issues/$number", Patch, Some(data))
  }

  @throws[ArtifactSourceException]
  def createIssueComment(repo: String, owner: String, number: Int, body: String): Comment = {
    val data = toJson(Map("body" -> body))
    httpRequest[Comment](s"$api/repos/$owner/$repo/issues/$number/comments", Post, Some(data))
  }

  @throws[ArtifactSourceException]
  def createTag(repo: String, owner: String, ctr: CreateTagRequest): Tag =
    httpRequest[Tag](s"$api/repos/$owner/$repo/git/tags", Post, Some(toJson(ctr)))

  @throws[ArtifactSourceException]
  def createRelease(repo: String, owner: String, tag: String, commitish: String, name: String, body: String): Release = {
    val data = toJson(Map("tag_name" -> tag, "target_commitish" -> commitish, "name" -> name, "body" -> body))
    httpRequest[Release](s"$api/repos/$owner/$repo/releases", Post, Some(data))
  }

  @throws[ArtifactSourceException]
  def createCommitCommentReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction("createCommitCommentReaction", s"$api/repos/$owner/$repo/comments/$id/reactions", content)

  def listCommitCommentReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/comments/$id/reactions", content)

  @throws[ArtifactSourceException]
  def createIssueReaction(repo: String, owner: String, number: Int, content: ReactionContent): Reaction =
    createReaction("createIssueReaction", s"$api/repos/$owner/$repo/issues/$number/reactions", content)

  def listIssueReactions(repo: String, owner: String, number: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/issues/$number/reactions", content)

  @throws[ArtifactSourceException]
  def createIssueCommentReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction("createIssueCommentReaction", s"$api/repos/$owner/$repo/issues/comments/$id/reactions", content)

  def listIssueCommentReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/issues/comments/$id/reactions", content)

  @throws[ArtifactSourceException]
  def createPullRequestReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction("createPullRequestReaction", s"$api/repos/$owner/$repo/issues/$id/reactions", content)

  def listPullRequestReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listIssueReactions(repo, owner, id, content)

  @throws[ArtifactSourceException]
  def createPullRequestReviewCommentReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction("createPullRequestReviewCommentReaction", s"$api/repos/$owner/$repo/pulls/comments/$id/reactions", content)

  def listPullRequestReviewCommentReactions(repo: String,
                                            owner: String,
                                            id: Int,
                                            content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/pulls/comments/$id/reactions", content)

  def listIssueEvents(repo: String,
                      owner: String,
                      number: Int,
                      params: Map[String, String] = Map("per_page" -> "100")): Seq[IssueEvent] =
    Try(paginateResults[IssueEvent](s"$api/repos/$owner/$repo/issues/$number/events", params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }

  private def createBlob(repo: String, owner: String, message: String, branch: String, fa: FileArtifact): GitHubRef = {
    val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
    val data = toJson(Map("content" -> content, "encoding" -> "base64"))
    httpRequest[GitHubRef](s"$api/repos/$owner/$repo/git/blobs", Post, Some(data))
  }

  private def createTree(repo: String, owner: String, treeEntries: Seq[TreeEntry]): Tree = {
    val data = toJson(Map("tree" -> treeEntries))
    httpRequest[Tree](s"$api/repos/$owner/$repo/git/trees", Post, Some(data))
  }

  private def createCommit(repo: String,
                           owner: String,
                           message: String,
                           treeSha: String,
                           parents: Seq[String]): CreateCommitResponse = {
    val data = toJson(Map("message" -> message, "tree" -> treeSha, "parents" -> parents))
    httpRequest[CreateCommitResponse](s"$api/repos/$owner/$repo/git/commits", Post, Some(data))
  }

  private def updateReference(repo: String, owner: String, ref: String, newSha: String): Reference = {
    val data = toJson(Map("sha" -> newSha, "force" -> true))
    httpRequest[Reference](s"$api/repos/$owner/$repo/git/refs/$ref", Patch, Some(data))
  }

  private def createWebhook(url: String,
                            name: String,
                            webhookUrl: String,
                            contentType: String,
                            active: Boolean,
                            events: Array[String]): Webhook = {
    val data = toJson(Map("name" -> name,
      "config" -> Map("url" -> webhookUrl, "content_type" -> contentType),
      "active" -> active,
      "events" -> events))
    httpRequest[Webhook](url, Post, Some(data))
  }

  private def createReaction(opName: String, url: String, content: ReactionContent): Reaction =
    httpRequest[Reaction](url, Post, Some(toJson(Map("content" -> content.toString))))

  private def listReactions(url: String, content: Option[ReactionContent] = None): Seq[Reaction] = {
    val params = content.map(rc => Map("content" -> rc.toString)).getOrElse(Map.empty) + ("per_page" -> "100")
    Try(paginateResults[Reaction](url, params)) match {
      case Success(results) => results
      case Failure(e) =>
        logger.debug(e.getMessage)
        Nil
    }
  }

  private def paginateResults[T](url: String,
                                 params: Map[String, String] = Map("per_page" -> "100"))
                                (implicit m: Manifest[T]): Seq[T] = {
    def nextPage(url: String, accumulator: Seq[T]): Seq[T] = {
      retry(url) {
        val resp = Http(url).headers(headers).params(params).asString.throwError
        val pages = accumulator ++ fromJson[Seq[T]](resp.body)
        if (params.keySet.contains("page")) pages
        else getNextUrl(resp).map(nextPage(_, pages)).getOrElse(pages)
      }
    }

    nextPage(url, Nil)
  }

  private def paginateSearchResults[T](url: String,
                                       params: Map[String, String] = Map("per_page" -> "100"))
                                      (implicit m: Manifest[T]): SearchResult[T] = {
    def nextPage(url: String, accumulator: Seq[T]): SearchResult[T] = {
      retry(url) {
        val resp = Http(url).headers(headers).params(params).asString.throwError
        val result = fromJson[SearchResult[T]](resp.body)
        val pages = accumulator ++ result.items
        val nextUrl = getNextUrl(resp)
        val nextPageNum = nextUrl.map(getPage).getOrElse(0)
        val lastPageNum = getLastUrl(resp).map(getPage).getOrElse(0)

        if (params.keySet.contains("page"))
          SearchResult(result.totalCount, result.incompleteResults, nextPageNum, lastPageNum, pages)
        else
          nextUrl match {
            case Some(nextPageUrl) => nextPage(nextPageUrl, pages)
            case None => SearchResult(result.totalCount, result.incompleteResults, nextPageNum, lastPageNum, pages)
          }
      }
    }

    nextPage(url, Nil)
  }

  private def getNextUrl(resp: HttpResponse[String]): Option[String] =
    resp.header("Link").flatMap(parseLinkHeader(_).get("next"))

  private def getLastUrl(resp: HttpResponse[String]): Option[String] =
    resp.header("Link").flatMap(parseLinkHeader(_).get("last"))

  private def getPage(url: String): Int =
    URLEncodedUtils.parse(url, defaultCharset()).asScala
      .find(_.getName == "page")
      .map(_.getValue)
      .getOrElse("0")
      .toInt
}

object GitHubServices {

  val Url = "https://github.com"
  val ApiUrl = "https://api.github.com"

  def apply(oAuthToken: String, apiUrl: String): GitHubServices =
    new GitHubServices(oAuthToken, apiUrl)

  def parseLinkHeader(linkHeader: String): Map[String, String] =
    if (linkHeader == null || linkHeader.isEmpty)
      Map.empty
    else
      linkHeader.split(',').map { part =>
        val section = part.split(';')
        val url = section(0).replace("<", "").replace(">", "").trim
        val name = section(1).replace(" rel=\"", "").replace("\"", "").trim
        (name, url)
      }.toMap
}
