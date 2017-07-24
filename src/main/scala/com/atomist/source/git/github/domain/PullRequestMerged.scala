package com.atomist.source.git.github.domain

case class PullRequestMerged(sha: String, merged: Boolean, message: String)
