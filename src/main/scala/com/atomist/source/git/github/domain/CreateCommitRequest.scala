package com.atomist.source.git.github.domain

private[github] case class CreateCommitRequest(message: String, tree: String, parents: Seq[String])