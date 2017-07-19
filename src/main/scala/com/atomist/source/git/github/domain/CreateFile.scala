package com.atomist.source.git.github.domain

case class CreateFile(path: String, message: String, content: String, branch: String)
