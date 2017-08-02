package com.atomist.source.git.domain

case class PullRequestMerged(sha: String, merged: Boolean, message: String)
