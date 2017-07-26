package com.atomist.source.git.github

import java.io.File

import com.atomist.source.ArtifactSourceException
import org.scalatest.{FlatSpec, Matchers}

class NaiveFileSystemGitHubSourceReaderTest extends FlatSpec with Matchers {

  def fakeReposForTest(): File = {
    new File(getClass.getClassLoader.getResource("fake_repos_for_test").toURI)
  }

  "NaiveFileSystemGitHubSourceReader" should "find cloned repository" in {
    val ghloc = GitHubArtifactSourceLocator.fromStrings("pretend", "notcheckingthis")
    val fsr = new NaiveFileSystemGitHubSourceReader(fakeReposForTest())
    val result = fsr.sourceFor(ghloc)

    result.totalFileCount should be > 0
    val readMe = result findFile "README.md"
    readMe shouldBe defined
    result.id match {
      case gi: GitHubArtifactSourceIdentifier => ghloc should equal(gi)
      case x => fail(s"Unexpected id $x")
    }
  }

  it should "not find uncloned repository" in {
    val asl = GitHubArtifactSourceLocator.fromStrings("somerepo", "someuser")
    val fsr = new NaiveFileSystemGitHubSourceReader(fakeReposForTest())
    an[ArtifactSourceException] should be thrownBy fsr.sourceFor(asl)
  }

  it should "throw exception if basedir doesn't exist" in {
    val asl = GitHubArtifactSourceLocator.fromStrings("somerepo", "someuser")
    val fsr = new NaiveFileSystemGitHubSourceReader(new File("what/in/gods/holy/name/are/you/blathering/about"))
    an[ArtifactSourceException] should be thrownBy fsr.sourceFor(asl)
  }
}
