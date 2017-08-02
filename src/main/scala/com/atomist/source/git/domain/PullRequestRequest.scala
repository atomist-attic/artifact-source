package com.atomist.source.git.domain

case class PullRequestRequest(title: String, head: String, base: String, body: String)
