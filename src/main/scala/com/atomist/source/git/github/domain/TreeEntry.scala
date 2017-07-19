package com.atomist.source.git.github.domain

private[github] case class TreeEntry(path: String, mode: String, `type`: String, sha: String)