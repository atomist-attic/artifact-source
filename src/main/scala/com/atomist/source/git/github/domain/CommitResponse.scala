package com.atomist.source.git.github.domain

case class CommitResponse(sha: String, url: String, tree: Ref, message: String, parents: Seq[Ref])