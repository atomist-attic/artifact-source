package com.atomist.source.git.github

import java.io.InputStream
import java.nio.charset.Charset
import java.util.{List => JList}

import com.atomist.source.git.DoNotRetryException
import com.atomist.source.git.github.domain.ReactionContent.ReactionContent
import com.atomist.source.git.github.domain._
import com.atomist.source.{ArtifactSourceException, FileArtifact, _}
import com.atomist.util.JsonUtils.{fromJson, toJson}
import com.atomist.util.Octal.intToOctal
import com.atomist.source.git.Retry.retry
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import resource._

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scalaj.http.{Base64, Http, HttpResponse}

case class GitHubServices(oAuthToken: String, apiUrl: Option[String] = None)
  extends GitHubSourceReader
    with LazyLogging {

  def this(oAuthToken: String, apiUrl: String) = this(oAuthToken, Option(apiUrl)) // For Java

  import GitHubServices._
  import HttpMethod._

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
      "Accept" -> "application/vnd.github.v3+json,application/vnd.github.loki-preview+json,application/vnd.github.squirrel-girl-preview"
    )

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = TreeGitHubArtifactSource(id, this)

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = TreeGitHubArtifactSource(id, this)

  def getOrganization(owner: String): Option[Organization] =
    retry("getOrganization") {
      httpRequestOption[Organization](s"$api/orgs/$owner")
    }

  def getRepository(repo: String, owner: String): Option[Repository] =
    Try(httpRequest[Repository](s"$api/repos/$owner/$repo")) match {
      case Success(r) => Some(r)
      case Failure(e) =>
        logger.warn(s"Failed to find user repository $owner/$repo. Searching for organization repository ...", e)
        paginateResults[Repository](s"$api/orgs/$owner/repos", Map("per_page" -> "100"))
          .find(_.name == repo)
    }

  def searchRepositories(params: Map[String, String] = Map("per_page" -> "100")): Seq[Repository] =
    paginateSearchResults[Repository](s"$api/search/repositories", params)

  @throws[ArtifactSourceException]
  def createRepository(repo: String,
                       owner: String,
                       description: String = "",
                       privateFlag: Boolean = false,
                       issues: Boolean = true,
                       autoInit: Boolean = false): Repository = {
    val url = getOrganization(owner).map(_ => s"$api/orgs/$owner/repos").getOrElse(s"$api/user/repos")
    val data = Map("name" -> repo, "owner" -> owner, "description" -> description, "private" -> privateFlag, "has_issues" -> issues, "auto_init" -> autoInit)
    retry("createRepository") {
      httpRequest[Repository](url, Post, Some(toJson(data)))
    }
  }

  @throws[ArtifactSourceException]
  def deleteRepository(repo: String, owner: String): Unit =
    retry("deleteRepository") {
      httpRequest[Unit](s"$api/repos/$owner/$repo", Delete)
    }

  def getBranch(repo: String, owner: String, branch: String): Option[Branch] =
    retry("getBranch") {
      httpRequestOption[Branch](s"$api/repos/$owner/$repo/branches/$branch")
    }

  def listBranches(repo: String, owner: String): Seq[Branch] =
    paginateResults[Branch](s"$api/repos/$owner/$repo/branches")

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
    retry("deleteBranch") {
      httpRequest[Unit](s"$api/repos/$owner/$repo/git/refs/heads/$branchName", Delete)
    }

  @throws[ArtifactSourceException]
  def createReference(repo: String, owner: String, ref: String, sha: String): Reference =
    retry("createReference") {
      httpRequest[Reference](s"$api/repos/$owner/$repo/git/refs", Post, Some(toJson(Map("ref" -> s"refs/heads/$ref", "sha" -> sha))))
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
    retry("createPullRequest") {
      httpRequest[PullRequest](s"$api/repos/$owner/$repo/pulls", Post, Some(toJson(prr)))
    }

  @throws[ArtifactSourceException]
  def mergePullRequest(repo: String,
                       owner: String,
                       number: Int,
                       title: String,
                       message: String,
                       mergeMethod: String = "squash"): PullRequestMerged =
    retry("mergePullRequest") {
      val data = Map("commit_title" -> title, "commit_message" -> message, "merge_method" -> mergeMethod)
      httpRequest[PullRequestMerged](s"$api/repos/$owner/$repo/pulls/$number/merge", Put, Some(toJson(data)))
    }

  def getPullRequest(repo: String, owner: String, number: Int): Option[PullRequest] =
    retry("getPullRequest") {
      httpRequestOption[PullRequest](s"$api/repos/$owner/$repo/pulls/$number")
    }

  def listPullRequests(repo: String,
                       owner: String,
                       state: String = PullRequest.Open,
                       sort: String = "created",
                       direction: String = "asc"): Seq[PullRequest] =
    retry("listPullRequests") {
      val params = Map("state" -> state, "sort" -> sort, "direction" -> direction, "per_page" -> "100")
      paginateResults[PullRequest](s"$api/repos/$owner/$repo/pulls", params)
    }

  @throws[ArtifactSourceException]
  def createPullRequestReviewComment(repo: String,
                                     owner: String,
                                     number: Int,
                                     body: String,
                                     commitId: String,
                                     path: String,
                                     position: Int): ReviewComment =
    retry("createPullRequestReviewComment") {
      val data = Map("body" -> body, "commit_id" -> commitId, "path" -> path, "position" -> position)
      httpRequest[ReviewComment](s"$api/repos/$owner/$repo/pulls/$number/comments", Post, Some(toJson(data)))
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
    retry("addOrUpdateFile") {
      val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
      val data = Map("path" -> fa.path, "message" -> message, "content" -> content, "branch" -> branch) ++
        fa.uniqueId.map(sha => Seq("sha" -> sha)).getOrElse(Nil)
      Try(httpRequest[CreateOrUpdateFileResponse](s"$api/repos/$owner/$repo/contents/${fa.path}", Put, Some(toJson(data)))) match {
        case Success(cfr) => fa.withUniqueId(cfr.content.sha)
        case Failure(e) => throw e
      }
    }
  }

  def getFileContents(repo: String, owner: String, path: String): Seq[Content] =
    retry("getFileContents") {
      httpRequestOption[Seq[Content]](s"$api/repos/$owner/$repo/contents/$path").getOrElse(Nil)
    }

  def getCommit(repo: String, owner: String, sha: String): Option[Commit] =
    retry("getCommit") {
      httpRequestOption[Commit](s"$api/repos/$owner/$repo/git/commits/$sha")
    }

  def listCommits(repo: String, owner: String, sha: Option[String] = None): Seq[Commit] =
    retry("listCommits") {
      val params = sha.map(s => Map("sha" -> s)).getOrElse(Map.empty) + ("per_page" -> "100")
      paginateResults[Commit](s"$api/repos/$owner/$repo/commits", params)
    }

  @throws[ArtifactSourceException]
  def createCommitComment(repo: String,
                          owner: String,
                          sha: String,
                          body: String,
                          path: String,
                          position: Int): CommitComment =
    retry("createCommitComment") {
      val data = Map("body" -> body, "path" -> path, "position" -> position)
      httpRequest[CommitComment](s"$api/repos/$owner/$repo/commits/$sha/comments", Post, Some(toJson(data)))
    }

  @throws[ArtifactSourceException]
  def getTreeRecursive(repo: String, owner: String, sha: String): Tree =
    retry("getTreeRecursive") {
      httpRequest[Tree](s"$api/repos/$owner/$repo/git/trees/$sha", params = Map("recursive" -> "1"))
    }

  @throws[ArtifactSourceException]
  def readBlob(repo: String, owner: String, sha: String): InputStream =
    retry("readBlob") {
      Try(Http(s"$api/repos/$owner/$repo/git/blobs/$sha")
        .timeout(connTimeoutMs = 2000, readTimeoutMs = 10000)
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
    retry("createWebhook") {
      createWebhook(s"$api/repos/$owner/$repo/hooks", name, url, contentType, active, events)
    }

  @throws[ArtifactSourceException]
  def createOrganizationWebhook(owner: String,
                                name: String,
                                url: String,
                                contentType: String,
                                active: Boolean,
                                events: Array[String]): Webhook =
    retry("createOrganizationWebhook") {
      createWebhook(s"$api/orgs/$owner/hooks", name, url, contentType, active, events)
    }

  def testWebhook(repo: String, owner: String, id: Int): Unit =
    httpRequestOption[Unit](s"$api/repos/$owner/$repo/hooks/$id/tests", Post, Some("".getBytes))

  @throws[ArtifactSourceException]
  def addCollaborator(repo: String, owner: String, collaborator: String): Unit =
    retry("addCollaborator") {
      httpRequestOption[Unit](s"$api/repos/$owner/$repo/collaborators/$collaborator", Put, params = Map("permission" -> "push"))
    }

  def getIssue(repo: String, owner: String, number: Int): Option[Issue] =
    retry("getIssue") {
      httpRequestOption[Issue](s"$api/repos/$owner/$repo/issues/$number")
    }

  def listIssues(params: Map[String, String] = Map("per_page" -> "100")): Seq[Issue] =
    paginateResults[Issue](s"$api/issues", params)

  def searchIssues(params: Map[String, String] = Map("per_page" -> "100")): Seq[Issue] =
    paginateSearchResults[Issue](s"$api/search/issues", params)

  @throws[ArtifactSourceException]
  def createIssue(repo: String,
                  owner: String,
                  title: String,
                  body: String,
                  labels: Seq[String] = Seq.empty,
                  assignees: Seq[String] = Seq.empty): Issue =
    retry("createIssue") {
      val data = Map("title" -> title, "body" -> body, "labels" -> labels, "assignees" -> assignees)
      httpRequest[Issue](s"$api/repos/$owner/$repo/issues", Post, Some(toJson(data)))
    }

  @throws[ArtifactSourceException]
  def editIssue(repo: String,
                owner: String,
                number: Int,
                title: String,
                body: String,
                state: String = "open",
                labels: Seq[String] = Seq.empty,
                assignees: Seq[String] = Seq.empty): Issue =
    retry("editIssue") {
      val data = Map("title" -> title, "body" -> body, "state" -> state, "labels" -> labels, "assignees" -> assignees)
      httpRequest[Issue](s"$api/repos/$owner/$repo/issues/$number", Patch, Some(toJson(data)))
    }

  @throws[ArtifactSourceException]
  def createIssueComment(repo: String, owner: String, number: Int, body: String): Comment =
    retry("createIssueComment") {
      httpRequest[Comment](s"$api/repos/$owner/$repo/issues/$number/comments", Post, Some(toJson(Map("body" -> body))))
    }

  @throws[ArtifactSourceException]
  def createTag(repo: String, owner: String, ctr: CreateTagRequest): Tag =
    retry("createTag") {
      httpRequest[Tag](s"$api/repos/$owner/$repo/git/tags", Post, Some(toJson(ctr)))
    }

  @throws[ArtifactSourceException]
  def createRelease(repo: String, owner: String, tag: String, commitish: String, name: String, body: String): Release =
    retry("createRelease") {
      val data = Map("tag_name" -> tag, "target_commitish" -> commitish, "name" -> name, "body" -> body)
      httpRequest[Release](s"$api/repos/$owner/$repo/releases", Post, Some(toJson(data)))
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

  def listPullRequestReviewCommentReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/pulls/comments/$id/reactions", content)

  private def createBlob(repo: String, owner: String, message: String, branch: String, fa: FileArtifact): GitHubRef =
    retry("createBlob") {
      val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
      val data = Map("content" -> content, "encoding" -> "base64")
      httpRequest[GitHubRef](s"$api/repos/$owner/$repo/git/blobs", Post, Some(toJson(data)))
    }

  private def createTree(repo: String, owner: String, treeEntries: Seq[TreeEntry]): Tree =
    retry("createTree") {
      httpRequest[Tree](s"$api/repos/$owner/$repo/git/trees", Post, Some(toJson(Map("tree" -> treeEntries))))
    }

  private def createCommit(repo: String,
                           owner: String,
                           message: String,
                           treeSha: String,
                           parents: Seq[String]): CreateCommitResponse =
    retry("createCommit") {
      val data = Map("message" -> message, "tree" -> treeSha, "parents" -> parents)
      httpRequest[CreateCommitResponse](s"$api/repos/$owner/$repo/git/commits", Post, Some(toJson(data)))
    }

  private def updateReference(repo: String, owner: String, ref: String, newSha: String): Reference =
    retry("updateReference") {
      val data = Map("sha" -> newSha, "force" -> true)
      httpRequest[Reference](s"$api/repos/$owner/$repo/git/refs/$ref", Patch, Some(toJson(data)))
    }

  private def createWebhook(url: String,
                            name: String,
                            webhookUrl: String,
                            contentType: String,
                            active: Boolean,
                            events: Array[String]): Webhook =
    retry("createWebhook") {
      val data = Map("name" -> name, "config" -> Map("url" -> webhookUrl, "content_type" -> contentType), "active" -> active, "events" -> events)
      httpRequest[Webhook](url, Post, Some(toJson(data)))
    }

  private def createReaction(opName: String, url: String, content: ReactionContent): Reaction =
    retry(opName) {
      httpRequest[Reaction](url, Post, Some(toJson(Map("content" -> content.toString))))
    }

  private def listReactions(url: String, content: Option[ReactionContent] = None): Seq[Reaction] = {
    val params = content.map(rc => Map("content" -> rc.toString)).getOrElse(Map.empty) + ("per_page" -> "100")
    paginateResults[Reaction](url, params)
  }

  private def httpRequestOption[T](url: String,
                                   method: HttpMethod = Get,
                                   data: Option[Array[Byte]] = None,
                                   params: Map[String, String] = Map.empty)(implicit m: Manifest[T]): Option[T] =
    Try(httpRequest(url, method, data, params)) match {
      case Success(res) => Some(res)
      case Failure(e) =>
        logger.warn(e.getMessage, e)
        None
    }

  private def httpRequest[T](url: String,
                             method: HttpMethod = Get,
                             data: Option[Array[Byte]] = None,
                             params: Map[String, String] = Map.empty)(implicit m: Manifest[T]): T =
    (method match {
      case Get => Http(url)
      case Post => data.map(Http(url).postData(_)).getOrElse(Http(url).method(method.toString))
      case Put => data.map(Http(url).put(_)).getOrElse(Http(url).method(method.toString))
      case Patch => data.map(Http(url).postData(_).method(method.toString)).getOrElse(Http(url).method(method.toString))
      case _ => Http(url).method(method.toString)
    }).headers(headers)
      .params(params)
      .exec((code: Int, headers: Map[String, IndexedSeq[String]], is: InputStream) => code match {
        case 200 | 201 => fromJson[T](is)
        case 204 => ().asInstanceOf[T]
        case 401 | 403 => throw DoNotRetryException(s"${headers("Status").head}")
        case _ => throw ArtifactSourceException(s"${headers("Status").head}, ${IOUtils.toString(is, Charset.defaultCharset)}")
      }).body

  private def paginateResults[T](url: String,
                                 params: Map[String, String] = Map("per_page" -> "100"))(implicit m: Manifest[T]): Seq[T] = {
    def nextPage(url: String, accumulator: Seq[T]): Seq[T] = {
      val resp = Http(url).headers(headers).params(params).asString
      if (resp.isSuccess) {
        val pages = accumulator ++ fromJson[Seq[T]](resp.body)
        getNextUrl(resp).map(nextPage(_, pages)).getOrElse(pages)
      } else {
        logger.debug(resp.body)
        Seq.empty
      }
    }

    nextPage(url, Seq.empty)
  }

  private def paginateSearchResults[T](url: String,
                                       params: Map[String, String] = Map("per_page" -> "100"))(implicit m: Manifest[T]): Seq[T] = {
    def nextPage(url: String, accumulator: Seq[T]): Seq[T] = {
      val resp = Http(url).headers(headers).params(params).asString
      if (resp.isSuccess) {
        val pages = accumulator ++ fromJson[SearchResult[T]](resp.body).items
        if (params.keySet.contains("page"))
          pages
        else
          getNextUrl(resp).map(nextPage(_, pages)).getOrElse(pages)
      } else
        Seq.empty
    }

    nextPage(url, Seq.empty)
  }

  private def getNextUrl(resp: HttpResponse[String]): Option[String] =
    resp.header("Link").flatMap(parseLinkHeader(_).get("next"))
}

object GitHubServices {

  val Url = "https://github.com"
  val ApiUrl = "https://api.github.com"

  def apply(oAuthToken: String, apiUrl: String): GitHubServices =
    new GitHubServices(oAuthToken, apiUrl)

  def parseLinkHeader(linkHeader: String): Map[String, String] =
    if (linkHeader == null || linkHeader.isEmpty) Map.empty
    else
      linkHeader.split(',').map { part =>
        val section = part.split(';')
        val url = section(0).replace("<", "").replace(">", "").trim
        val name = section(1).replace(" rel=\"", "").replace("\"", "").trim
        (name, url)
      }.toMap
}

object HttpMethod extends Enumeration {

  type HttpMethod = Value

  val Head = Value("HEAD")
  val Get = Value("GET")
  val Post = Value("POST")
  val Patch = Value("PATCH")
  val Put = Value("PUT")
  val Delete = Value("DELETE")
}
