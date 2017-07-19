package com.atomist.source.git.github.domain

case class PullRequestRequest(title: String, head: String, base: String, body: String)
