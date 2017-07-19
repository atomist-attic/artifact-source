package com.atomist.source.git.github.domain

case class CreateTreeResponse(sha: String, url: String, tree: Seq[TreeElement])