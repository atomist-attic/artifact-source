package com.atomist.source.git.github

import com.atomist.source._
import com.typesafe.scalalogging.LazyLogging

case class GitHubArtifactSourceWriter(oAuthToken: String, apiUrl: Option[String] = None)
  extends LazyLogging {

  def this(oAuthToken: String, apiUrl: String) = this(oAuthToken, Option(apiUrl)) // For Java

  import GitHubArtifactSourceWriter._

  private val ghs = GitHubServices(oAuthToken, apiUrl)

  @throws[ArtifactSourceException]
  def write(as: ArtifactSource, sui: GitHubSourceUpdateInfo): Seq[FileArtifact] =
    if (as.allFiles.size == 1) {
      Seq(ghs.addOrUpdateFile(sui, as.allFiles.head))
    } else if (as.findFile(ProvenanceFileName).isDefined) {
      ghs commitFiles(sui, as.allFiles, Seq.empty)
    } else {
      val newAs = as + StringFileArtifact(ProvenanceFileName, "")
      ghs commitFiles(sui, newAs.allFiles, Seq.empty)
    }

  @throws[ArtifactSourceException]
  def writeNewRepo(as: ArtifactSource,
                   sui: GitHubSourceUpdateInfo,
                   cri: CloudRepoId,
                   description: String = "",
                   privateFlag: Boolean = false,
                   issues: Boolean = true,
                   autoInit: Boolean = false): Seq[FileArtifact] = {
    ghs createRepository(cri.repo, cri.owner, description, privateFlag, issues, autoInit)
    write(as, sui)
  }
}

object GitHubArtifactSourceWriter {

  private val ProvenanceFileName = ".atomist.yml"
}


