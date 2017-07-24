package com.atomist.source.git.github.domain

case class Tag(tag: String, sha: String, url: String, message: String, tagger: Tagger, `object`: TagObject)

