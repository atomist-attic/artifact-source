package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonFormat

case class Tagger(name: String,
                  email: String,
                  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ssXXX") date: OffsetDateTime)