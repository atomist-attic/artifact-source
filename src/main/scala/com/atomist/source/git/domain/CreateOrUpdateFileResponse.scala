package com.atomist.source.git.domain

private[git] case class CreateOrUpdateFileResponse(content: Commit, commit: Commit)