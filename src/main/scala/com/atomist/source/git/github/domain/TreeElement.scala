package com.atomist.source.git.github.domain

case class TreeElement(path: String, mode: String, `type`: String, size: Int, sha: String, url: String)
