package com.atomist.source.git.domain

case class CreateTagRequest(tag: String, message: String, `object`: String, `type`: String, tagger: Tagger)
