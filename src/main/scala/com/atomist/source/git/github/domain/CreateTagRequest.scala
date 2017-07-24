package com.atomist.source.git.github.domain

case class CreateTagRequest(tag: String, message: String, `object`: String, `type`: String, tagger: Tagger)
