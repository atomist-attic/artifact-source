package com.atomist.source.git

import com.atomist.source.{ArtifactSource, ArtifactSourceAccessException, EmptyArtifactSource}
import org.scalatest.{FlatSpec, Matchers}

class MultiStrategyGitHubSourceReaderTest extends FlatSpec with Matchers {

  private val gitHubLocatorThatWontBeFoundOnGitHub = GitHubArtifactSourceLocator.fromStrings("owner", "repository")

  "MultiStrategyGitHubSourceReader" should "always fail with no readers" in {
    val msg = new MultiStrategyGitHubSourceReader(Seq.empty)
    an[ArtifactSourceAccessException] should be thrownBy msg.sourceFor(gitHubLocatorThatWontBeFoundOnGitHub)
  }

  it should "fail with one failing reader" in {
    val msg = new MultiStrategyGitHubSourceReader(Seq(AlwaysFailsReader))
    an[ArtifactSourceAccessException] should be thrownBy msg.sourceFor(gitHubLocatorThatWontBeFoundOnGitHub)
  }

  it should "succeed with at least one successful reader" in {
    val msg = new MultiStrategyGitHubSourceReader(Seq(AlwaysFailsReader, new AlwaysSucceedsReader))
    msg.sourceFor(gitHubLocatorThatWontBeFoundOnGitHub).getIdString should not be null
  }

  it should "not invoke after one successful reader" in {
    val asr = new AlwaysSucceedsReader
    val msg = new MultiStrategyGitHubSourceReader(Seq(AlwaysFailsReader, new AlwaysSucceedsReader, asr))
    msg.sourceFor(gitHubLocatorThatWontBeFoundOnGitHub).getIdString should not be null
    asr.invocations shouldBe 0
  }

  object AlwaysFailsReader extends GitHubSourceReader {

    override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = ???

    override def treeFor(id: GitHubShaIdentifier): ArtifactSource = ???
  }

  class AlwaysSucceedsReader extends GitHubSourceReader {
    var invocations = 0

    override def sourceFor(loc: GitHubArtifactSourceLocator): ArtifactSource = {
      invocations += 1
      val fakeId = GitHubArtifactSourceIdentifier(loc, "thisIsABogusCommitShaButItDoesntMatter")
      EmptyArtifactSource(fakeId)
    }

    override def treeFor(id: GitHubShaIdentifier): ArtifactSource = {
      invocations += 1
      EmptyArtifactSource(id)
    }
  }
}
