package com.atomist.source.git.github

import com.atomist.source._
import com.atomist.source.git.TestConstants
import com.typesafe.scalalogging.LazyLogging
import org.kohsuke.github.GHRepository
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FlatSpec, Matchers}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/**
  * Superclass for tests that mutate GitHub.
  */
abstract class GitHubMutatorTest(val oAuthToken: String)
  extends FlatSpec
    with Matchers
    with BeforeAndAfter
    with BeforeAndAfterAll
    with LazyLogging {

  import TestConstants._

  protected def placeholderFilename(testName: String) = s"${testName}_${System.currentTimeMillis}.txt"

  protected val ghs = GitHubServices(oAuthToken)
  protected val testFileContents = "The quick brown fox jumped over the lazy dog"
  protected val testFiles: Seq[FileArtifact] = Seq(
    StringFileArtifact("somethingOrOther.txt", testFileContents),
    StringFileArtifact("scripts2/other.txt", "This file isn't in the root"),
    StringFileArtifact("another2/directory/tree/extra.txt", "Nested file")
  )

  override protected def afterAll(): Unit = cleanUp()

  /**
    * Return a temporary repository callers can use.
    */
  def newTemporaryRepo(autoInit: Boolean = false): GHRepository =
    ghs createRepository(getRepoName, TestOrg, "temporary test repository", privateFlag = true, autoInit = autoInit)

  /**
    * Most callers will want a repository with something in it. Otherwise there isn't even a default branch,
    * so put in a README.md file by setting auto_init to true.
    */
  def newPopulatedTemporaryRepo(): GHRepository = newTemporaryRepo(true)

  /**
    * Clean up after the work of this class.
    */
  private def cleanUp() =
    Try(ghs.gitHub.searchRepositories().q(s"user:$TestOrg in:name $TemporaryRepoPrefix").list) match {
      case Success(repos) => repos.asScala.foreach(_.delete)
      case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
    }

  private def getRepoName = s"$TemporaryRepoPrefix${System.nanoTime}"
}
