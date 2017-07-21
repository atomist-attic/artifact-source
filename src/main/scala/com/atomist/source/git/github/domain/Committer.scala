package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

case class Committer(name: String, email: String, date: OffsetDateTime)

