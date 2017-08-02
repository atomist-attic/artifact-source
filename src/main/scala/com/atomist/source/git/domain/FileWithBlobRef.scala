package com.atomist.source.git.domain

import com.atomist.source.FileArtifact

private[git] case class FileWithBlobRef(fa: FileArtifact, ref: GitHubRef)