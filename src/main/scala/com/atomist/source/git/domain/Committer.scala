package com.atomist.source.git.domain

import java.time.OffsetDateTime

case class Committer(name: String, email: String, date: OffsetDateTime)

