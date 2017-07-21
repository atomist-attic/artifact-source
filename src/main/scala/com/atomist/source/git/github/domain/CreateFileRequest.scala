package com.atomist.source.git.github.domain

private[github] case class CreateFileRequest(path: String, message: String, content: String, branch: String)
