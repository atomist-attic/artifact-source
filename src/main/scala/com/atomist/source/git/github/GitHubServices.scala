package com.atomist.source.git.github

import java.io.InputStream
import java.nio.charset.Charset
import java.util.{List => JList}

import com.atomist.source.git.github.domain.ReactionContent.ReactionContent
import com.atomist.source.git.github.domain._
import com.atomist.source.{ArtifactSourceUpdateException, FileArtifact, _}
import com.atomist.util.JsonUtils.{fromJson, toJson}
import com.atomist.util.Octal.intToOctal
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
    Try(Http(s"$api/orgs/$owner")
      .headers(headers)
      .execute(fromJson[Organization])
      .throwError
      .body) match {
      case Success(org) => Some(org)
      case Failure(e) =>
        logger.debug(s"Failed to find organization $owner", e)
        None
    }

  def getRepository(repo: String, owner: String): Option[Repository] =
    Try(Http(s"$api/repos/$owner/$repo")
      .headers(headers)
      .execute(fromJson[Repository])
      .throwError
      .body) match {
      case Success(r) => Some(r)
      case Failure(e) =>
        logger.warn(s"Failed to find user repository $owner/$repo. Searching for organization repository", e)
        val params = Map("per_page" -> "100")
        val resp = Http(s"$api/orgs/$owner/repos").headers(headers).params(params).asString
        if (resp.isSuccess) {
          val firstPage = fromJson[Seq[Repository]](resp.body)
          getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage).find(_.name == repo)
        } else {
          logger.warn(s"Failed to find user or organization repository $owner/$repo: ${resp.body}")
          None
        }
    }

  def searchRepositories(params: Map[String, String] = Map("per_page" -> "100")): Seq[Repository] = {
    val resp = Http(s"$api/search/repositories").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[SearchResult[Repository]](resp.body).items
      getNextUrl(resp).map(paginateSearchResults(_, firstPage, params)).getOrElse(firstPage)
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
      val url = getOrganization(owner).map(_ => s"$api/orgs/$owner/repos").getOrElse(s"$api/user/repos")
      val map = Map("name" -> repo, "owner" -> owner, "description" -> description, "private" -> privateFlag, "has_issues" -> issues, "auto_init" -> autoInit)
      Http(url)
        .postData(toJson(map))
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
    Try(Http(s"$api/repos/$owner/$repo")
      .method("DELETE")
      .headers(headers)
      .asString
      .throwError
      .body
    ) match {
      case Success(_) => logger.debug(s"Successfully deleted repository $owner/$repo")
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to delete repository $owner/$repo", e)
    }

  def getBranch(repo: String, owner: String, branch: String): Option[Branch] =
    Try(Http(s"$api/repos/$owner/$repo/branches/$branch")
      .headers(headers)
      .execute(fromJson[Branch])
      .throwError
      .body) match {
      case Success(b) => Some(b)
      case Failure(e) =>
        logger.debug(s"Failed to find branch $branch in $owner/$repo", e)
        None
    }

  def listBranches(repo: String, owner: String): Seq[Branch] = {
    val resp = Http(s"$api/repos/$owner/$repo/branches").headers(headers).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[Seq[Branch]](resp.body)
      getNextUrl(resp).map(paginateResults(_, firstPage)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to list branches in $owner/$repo: ${resp.body}")
      Seq.empty
    }
  }

  @throws[ArtifactSourceUpdateException]
  def createBranch(repo: String, owner: String, branchName: String, fromBranch: String): Reference =
    Try(Http(s"$api/repos/$owner/$repo/git/refs/heads/$fromBranch")
      .headers(headers)
      .execute(fromJson[Reference])
      .throwError
      .body) match {
      case Success(fromRef) => createReference(repo, owner, branchName, fromRef.`object`.sha)
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
      case Failure(e) => throw ArtifactSourceUpdateException(e.getMessage, e)
    }

  @throws[ArtifactSourceUpdateException]
  def deleteBranch(repo: String, owner: String, branchName: String): Unit =
    Try(Http(s"$api/repos/$owner/$repo/git/refs/heads/$branchName")
      .method("DELETE")
      .headers(headers)
      .asString
      .throwError
      .body) match {
      case Success(_) => logger.debug(s"Successfully deleted branch $branchName in $owner/$repo")
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to delete branch $branchName in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def createReference(repo: String, owner: String, ref: String, sha: String): Reference = {
    Try(Http(s"$api/repos/$owner/$repo/git/refs")
      .postData(toJson(Map("ref" -> s"refs/heads/$ref", "sha" -> sha)))
      .headers(headers)
      .execute(fromJson[Reference])
      .throwError
      .body) match {
      case Success(reference) => reference
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create reference in $owner/$repo", e)
    }
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
    Try(Http(s"$api/repos/$owner/$repo/pulls")
      .postData(toJson(prr))
      .headers(headers)
      .execute(fromJson[PullRequest])
      .throwError
      .body) match {
      case Success(pr) => pr
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to create pull request for $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def mergePullRequest(repo: String,
                       owner: String,
                       number: Int,
                       title: String,
                       message: String,
                       mergeMethod: String = "squash"): PullRequestMerged =
    Try(Http(s"$api/repos/$owner/$repo/pulls/$number/merge")
      .postData(toJson(Map("commit_title" -> title, "commit_message" -> message, "merge_method" -> mergeMethod)))
      .method("PUT")
      .headers(headers)
      .execute(fromJson[PullRequestMerged])
      .throwError
      .body) match {
      case Success(prm) => prm
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to merge pull request $number in $owner/$repo", e)
    }

  def getPullRequest(repo: String, owner: String, number: Int): Option[PullRequest] =
    Try(Http(s"$api/repos/$owner/$repo/pulls/$number")
      .headers(headers)
      .execute(fromJson[PullRequest])
      .throwError
      .body) match {
      case Success(pr) => Some(pr)
      case Failure(e) =>
        logger.warn(s"Failed to get pull request $number in $owner/$repo", e)
        None
    }

  def listPullRequests(repo: String,
                       owner: String,
                       state: String = PullRequest.Open,
                       sort: String = "created",
                       direction: String = "asc"): Seq[PullRequest] = {
    val params = Map("state" -> state, "sort" -> sort, "direction" -> direction, "per_page" -> "100")
    val resp = Http(s"$api/repos/$owner/$repo/pulls").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[Seq[PullRequest]](resp.body)
      getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to list pull requests in $owner/$repo: ${resp.body}")
      Seq.empty
    }
  }

  @throws[ArtifactSourceUpdateException]
  def createPullRequestReviewComment(repo: String,
                                     owner: String,
                                     number: Int,
                                     body: String,
                                     commitId: String,
                                     path: String,
                                     position: Int): ReviewComment =
    Try(Http(s"$api/repos/$owner/$repo/pulls/$number/comments")
      .postData(toJson(Map("body" -> body, "commit_id" -> commitId, "path" -> path, "position" -> position)))
      .headers(headers)
      .execute(fromJson[ReviewComment])
      .throwError
      .body) match {
      case Success(rc) => rc
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create review comment for pull request $number in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def commitFiles(sui: GitHubSourceUpdateInfo,
                  files: JList[FileArtifact],
                  filesToDelete: JList[FileArtifact]): JList[FileArtifact] =
    commitFiles(sui, files.asScala, filesToDelete.asScala).asJava

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

      val commit = createCommit(repo, owner, message, tree.sha, Seq(baseTreeSha))
      updateReference(repo, owner, s"heads/$branch", commit.sha)

      filesWithBlobRefs.map(fileWithBlobRef => fileWithBlobRef.fa.withUniqueId(fileWithBlobRef.ref.sha))
    } match {
      case Success(fileArtifacts) => fileArtifacts
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to commit files to $owner/$repo", e)
    }
  }

  @throws[ArtifactSourceUpdateException]
  def addOrUpdateFile(sui: GitHubSourceUpdateInfo, fa: FileArtifact): FileArtifact = {
    val sourceId = sui.sourceId
    addOrUpdateFile(sourceId.repo, sourceId.owner, sourceId.branch, sui.message, fa)
  }

  @throws[ArtifactSourceUpdateException]
  def addOrUpdateFile(repo: String,
                      owner: String,
                      branch: String,
                      message: String,
                      fa: FileArtifact): FileArtifact =
    Try {
      val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
      val data = Map("path" -> fa.path, "message" -> message, "content" -> content, "branch" -> branch)
      Http(s"$api/repos/$owner/$repo/contents/${fa.path}")
        .postData(toJson(data ++ fa.uniqueId.map(sha => Seq("sha" -> sha)).getOrElse(Nil)))
        .method("PUT")
        .headers(headers)
        .execute(fromJson[CreateOrUpdateFileResponse])
        .throwError
        .body
    } match {
      case Success(cfr) => fa.withUniqueId(cfr.content.sha)
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to add or update file to $owner/$repo", e)
    }

  def getFileContents(repo: String, owner: String, path: String): Seq[Content] =
    Try(Http(s"$api/repos/$owner/$repo/contents/$path")
      .headers(headers)
      .execute(fromJson[Seq[Content]])
      .throwError
      .body) match {
      case Success(contents) => contents
      case Failure(e) =>
        logger.debug(s"Failed to get contents for path $path in $owner/$repo", e)
        Seq.empty
    }

  def getCommit(repo: String, owner: String, sha: String): Option[Commit] =
    Try(Http(s"$api/repos/$owner/$repo/git/commits/$sha")
      .headers(headers)
      .execute(fromJson[Commit])
      .throwError
      .body) match {
      case Success(commit) => Some(commit)
      case Failure(e) =>
        logger.debug(s"Failed to get commit $sha in $owner/$repo", e)
        None
    }

  def listCommits(repo: String, owner: String, sha: Option[String] = None): Seq[Commit] = {
    val params = sha.map(s => Map("sha" -> s)).getOrElse(Map.empty) + ("per_page" -> "100")
    val resp = Http(s"$api/repos/$owner/$repo/commits").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[Seq[Commit]](resp.body)
      getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage)
    } else {
      logger
        .warn(s"Failed to list commits in $owner/$repo: ${resp.body}")
      Seq.empty
    }
  }

  @throws[ArtifactSourceUpdateException]
  def createCommitComment(repo: String,
                          owner: String,
                          sha: String,
                          body: String,
                          path: String,
                          position: Int): CommitComment =
    Try(Http(s"$api/repos/$owner/$repo/commits/$sha/comments")
      .postData(toJson(Map("body" -> body, "path" -> path, "position" -> position)))
      .headers(headers)
      .execute(fromJson[CommitComment])
      .throwError
      .body) match {
      case Success(cc) => cc
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create commit comment for commit $sha in $owner/$repo", e)
    }

  @throws[ArtifactSourceAccessException]
  def getTreeRecursive(repo: String, owner: String, sha: String): Tree =
    Try(Http(s"$api/repos/$owner/$repo/git/trees/$sha")
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
    Try(Http(s"$api/repos/$owner/$repo/git/blobs/$sha")
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

  @throws[ArtifactSourceUpdateException]
  def createWebhook(repo: String,
                    owner: String,
                    name: String,
                    url: String,
                    contentType: String,
                    active: Boolean,
                    events: Array[String]): Webhook =
    createWebhook(s"$api/repos/$owner/$repo/hooks", name, url, contentType, active, events)

  @throws[ArtifactSourceUpdateException]
  def createOrganizationWebhook(owner: String,
                                name: String,
                                url: String,
                                contentType: String,
                                active: Boolean,
                                events: Array[String]): Webhook =
    createWebhook(s"$api/orgs/$owner/hooks", name, url, contentType, active, events)

  def testWebhook(repo: String, owner: String, id: Int): Unit =
    Try {
      val resp = Http(s"$api/repos/$owner/$repo/hooks/$id/tests").postData("").headers(headers).asString
      resp.code match {
        case 204 => logger.info(s"Successfully tested webhook in $owner/$repo")
        case _ => logger.warn(s"Failed to test webhook in $owner/$repo: ${resp.body}")
      }
    }

  @throws[ArtifactSourceUpdateException]
  def addCollaborator(repo: String, owner: String, collaborator: String): Unit =
    Try(Http(s"$api/repos/$owner/$repo/collaborators/$collaborator")
      .method("PUT")
      .param("permission", "push")
      .headers(headers)
      .asString
      .throwError) match {
      case Success(_) => logger.info(s"Successfully added collaborator $collaborator to $owner/$repo")
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to add collaborator $collaborator to $owner/$repo", e)
    }

  def getIssue(repo: String, owner: String, number: Int): Option[Issue] =
    Try(Http(s"$api/repos/$owner/$repo/issues/$number")
      .headers(headers)
      .execute(fromJson[Issue])
      .throwError
      .body) match {
      case Success(issue) => Some(issue)
      case Failure(e) =>
        logger.warn(s"Failed to get issue $number in $owner/$repo", e)
        None
    }

  def listIssues(params: Map[String, String] = Map("per_page" -> "100")): Seq[Issue] = {
    val resp = Http(s"$api/issues").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[Seq[Issue]](resp.body)
      getNextUrl(resp).map(paginateResults(_, firstPage, params)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to list issues")
      Seq.empty
    }
  }

  def searchIssues(params: Map[String, String] = Map("per_page" -> "100")): Seq[Issue] = {
    val resp = Http(s"$api/search/issues").headers(headers).params(params).asString
    if (resp.isSuccess) {
      val firstPage = fromJson[SearchResult[Issue]](resp.body).items
      getNextUrl(resp).map(paginateSearchResults(_, firstPage, params)).getOrElse(firstPage)
    } else {
      logger.warn(s"Failed to find issues: ${resp.body}")
      Seq.empty
    }
  }

  @throws[ArtifactSourceUpdateException]
  def createIssue(repo: String,
                  owner: String,
                  title: String,
                  body: String,
                  labels: Seq[String] = Seq.empty,
                  assignees: Seq[String] = Seq.empty): Issue =
    Try(Http(s"$api/repos/$owner/$repo/issues")
      .postData(toJson(Map("title" -> title, "body" -> body, "labels" -> labels, "assignees" -> assignees)))
      .headers(headers)
      .execute(fromJson[Issue])
      .throwError
      .body) match {
      case Success(issue) => issue
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create issue in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def editIssue(repo: String,
                owner: String,
                number: Int,
                title: String,
                body: String,
                state: String = "open",
                labels: Seq[String] = Seq.empty,
                assignees: Seq[String] = Seq.empty): Issue =
    Try(Http(s"$api/repos/$owner/$repo/issues/$number")
      .postData(toJson(Map("title" -> title, "body" -> body, "state" -> state, "labels" -> labels, "assignees" -> assignees)))
      .method("PATCH")
      .headers(headers)
      .execute(fromJson[Issue])
      .throwError
      .body) match {
      case Success(issue) => issue
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to edit issue $number in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def createIssueComment(repo: String, owner: String, number: Int, body: String): Comment =
    Try(Http(s"$api/repos/$owner/$repo/issues/$number/comments")
      .postData(toJson(Map("body" -> body)))
      .headers(headers)
      .execute(fromJson[Comment])
      .throwError
      .body) match {
      case Success(comment) => comment
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create comment for issue $number in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def createTag(repo: String, owner: String, ctr: CreateTagRequest): Tag =
    Try(Http(s"$api/repos/$owner/$repo/git/tags")
      .postData(toJson(ctr))
      .headers(headers)
      .execute(fromJson[Tag])
      .throwError
      .body) match {
      case Success(tag) => tag
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create tag ${ctr.tag} in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def createRelease(repo: String, owner: String, tag: String, commitish: String, name: String, body: String): Release =
    Try(Http(s"$api/repos/$owner/$repo/releases")
      .postData(toJson(Map("tag_name" -> tag, "target_commitish" -> commitish, "name" -> name, "body" -> body)))
      .headers(headers)
      .execute(fromJson[Release])
      .throwError
      .body) match {
      case Success(release) => release
      case Failure(e) =>
        throw ArtifactSourceUpdateException(s"Failed to create release for tag $tag in $owner/$repo", e)
    }

  @throws[ArtifactSourceUpdateException]
  def createCommitCommentReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction(s"$api/repos/$owner/$repo/comments/$id/reactions", content)

  def listCommitCommentReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/comments/$id/reactions", content)

  @throws[ArtifactSourceUpdateException]
  def createIssueReaction(repo: String, owner: String, number: Int, content: ReactionContent): Reaction =
    createReaction(s"$api/repos/$owner/$repo/issues/$number/reactions", content)

  def listIssueReactions(repo: String, owner: String, number: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/issues/$number/reactions", content)

  @throws[ArtifactSourceUpdateException]
  def createIssueCommentReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction(s"$api/repos/$owner/$repo/issues/comments/$id/reactions", content)

  def listIssueCommentReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/issues/comments/$id/reactions", content)

  @throws[ArtifactSourceUpdateException]
  def createPullRequestReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createIssueReaction(repo, owner, id, content)

  def listPullRequestReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listIssueReactions(repo, owner, id, content)

  @throws[ArtifactSourceUpdateException]
  def createPullRequestReviewCommentReaction(repo: String, owner: String, id: Int, content: ReactionContent): Reaction =
    createReaction(s"$api/repos/$owner/$repo/pulls/comments/$id/reactions", content)

  def listPullRequestReviewCommentReactions(repo: String, owner: String, id: Int, content: Option[ReactionContent] = None): Seq[Reaction] =
    listReactions(s"$api/repos/$owner/$repo/pulls/comments/$id/reactions", content)

  private def createBlob(repo: String, owner: String, message: String, branch: String, fa: FileArtifact): GitHubRef = {
    val content = managed(fa.inputStream()).acquireAndGet(is => new String(Base64.encode(IOUtils.toByteArray(is))))
    Http(s"$api/repos/$owner/$repo/git/blobs")
      .postData(toJson(Map("content" -> content, "encoding" -> "base64")))
      .headers(headers)
      .execute(fromJson[GitHubRef])
      .throwError
      .body
  }

  private def createTree(repo: String, owner: String, treeEntries: Seq[TreeEntry]): Tree =
    Http(s"$api/repos/$owner/$repo/git/trees")
      .postData(toJson(Map("tree" -> treeEntries)))
      .headers(headers)
      .exec((code: Int, headers: Map[String, IndexedSeq[String]], is: InputStream) => code match {
        case 201 => fromJson[Tree](is)
        case _ =>
          throw new IllegalArgumentException(s"${headers("Status").head}, ${IOUtils.toString(is, Charset.defaultCharset)}")
      }).body

  private def createCommit(repo: String,
                           owner: String,
                           message: String,
                           treeSha: String,
                           parents: Seq[String]): CreateCommitResponse =
    Http(s"$api/repos/$owner/$repo/git/commits")
      .postData(toJson(Map("message" -> message, "tree" -> treeSha, "parents" -> parents)))
      .headers(headers)
      .execute(fromJson[CreateCommitResponse])
      .throwError
      .body

  private def updateReference(repo: String, owner: String, ref: String, newSha: String): Unit =
    Http(s"$api/repos/$owner/$repo/git/refs/$ref")
      .postData(toJson(Map("sha" -> newSha, "force" -> true)))
      .method("PATCH")
      .headers(headers)
      .asBytes
      .throwError

  private def createWebhook(url: String,
                            name: String,
                            webhookUrl: String,
                            contentType: String,
                            active: Boolean,
                            events: Array[String]): Webhook =
    Try(Http(url)
      .postData(toJson(Map("name" -> name, "config" -> Map("url" -> webhookUrl, "content_type" -> contentType), "active" -> active, "events" -> events)))
      .headers(headers)
      .execute(fromJson[Webhook])
      .throwError
      .body) match {
      case Success(hook) => hook
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to create webhook", e)
    }

  private def createReaction(url: String, content: ReactionContent): Reaction =
    Try(Http(url)
      .postData(toJson(Map("content" -> content.toString)))
      .headers(headers)
      .execute(fromJson[Reaction])
      .throwError
      .body) match {
      case Success(reaction) => reaction
      case Failure(e) => throw ArtifactSourceUpdateException(s"Failed to create reaction", e)
    }

  private def listReactions(url: String, content: Option[ReactionContent] = None): Seq[Reaction] =
    Try(Http(url)
      .params(content.map(rc => Map("content" -> rc.toString)).getOrElse(Map.empty))
      .headers(headers)
      .execute(fromJson[Seq[Reaction]])
      .throwError
      .body) match {
      case Success(reactions) => reactions
      case Failure(e) =>
        logger.debug(s"Failed to list reactions", e)
        Seq.empty
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

  private def paginateSearchResults[T](url: String,
                                       firstPage: Seq[T],
                                       params: Map[String, String] = Map.empty)(implicit m: Manifest[T]): Seq[T] = {
    def nextPage(url: String, accumulator: Seq[T]): Seq[T] = {
      val resp = Http(url).headers(headers).params(params).asString
      if (resp.isSuccess) {
        val pages = accumulator ++ fromJson[SearchResult[T]](resp.body).items
        getNextUrl(resp).map(nextPage(_, pages)).getOrElse(pages)
      } else {
        logger.warn(s"Failed to find results: ${resp.body}")
        Seq.empty
      }
    }

    nextPage(url, firstPage)
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
