package com.atomist.source.git.github.domain

case class Tree(sha: String, url: String, tree: Seq[TreeElement], truncated: Boolean)
