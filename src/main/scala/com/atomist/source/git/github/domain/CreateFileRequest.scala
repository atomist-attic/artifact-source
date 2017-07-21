package com.atomist.source.git.github.domain

case class CreateFileRequest(path: String, message: String, content: String, branch: String)
