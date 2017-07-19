package com.atomist.source.git.github.domain

import com.atomist.source.FileArtifact

private[github] case class FileWithBlobRef(fa: FileArtifact, ref: GitHubRef)