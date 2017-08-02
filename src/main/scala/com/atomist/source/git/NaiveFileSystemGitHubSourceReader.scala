package com.atomist.source.git

import java.io.File

import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier}
import com.atomist.source.{Artifact, ArtifactSource, ArtifactSourceException, DirectoryBasedArtifactContainer}

/**
  * Implementation of [[GitHubSourceReader]] that looks for local
  * directories. As its name indicates, this is naive and not intended for use in
  * production, as it makes no attempt to verify that the directory contents
  * are in fact a cloned repository or, if so, are up to date or from the correct branch.
  * Commit sha returned is non-null but meaningless.
  */
class NaiveFileSystemGitHubSourceReader(directoryFullOfRepos: File) extends GitHubSourceReader {

  override def sourceFor(loc: GitHubArtifactSourceLocator): ArtifactSource = {
    if (!directoryFullOfRepos.isDirectory)
      throw ArtifactSourceException(s"$directoryFullOfRepos must be a directory containing cloned repos")

    val matchingDirectoryO = directoryFullOfRepos.listFiles.toIndexedSeq.find(dir => loc.repo.equals(dir.getName))

    matchingDirectoryO match {
      case None =>
        throw ArtifactSourceException(s"Directory with name ${loc.repo} was not found in base dir $directoryFullOfRepos")
      case Some(dir) =>
        val fakeId = GitHubArtifactSourceIdentifier(loc, "bogusCommitSha")
        val as = new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(dir))
        new ArtifactSource with DirectoryBasedArtifactContainer {
          override val id = fakeId
          override val artifacts: Seq[Artifact] = as.artifacts
        }
    }
  }

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = ???
}
