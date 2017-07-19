package com.atomist.source.git.github.domain

private[github] case class UpdateReferenceRequest(sha: String, force: Boolean)