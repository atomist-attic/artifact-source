package com.atomist.source.git.github.domain

private[github] case class CommitRequest(message: String, tree: String, parents: Seq[String])