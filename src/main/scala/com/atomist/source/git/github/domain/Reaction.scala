package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.atomist.source.git.github.domain.ReactionContent.ReactionContent
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.module.scala.JsonScalaEnumeration

case class Reaction(id: Int,
                    user: User,
                    @JsonScalaEnumeration(classOf[ReactionContentType]) content: ReactionContent,
                    @JsonProperty("created_at") createdAt: OffsetDateTime)

class ReactionContentType extends TypeReference[ReactionContent.type]

object ReactionContent extends Enumeration {

  type ReactionContent = Value

  val PlusOne = Value("+1")
  val MinusOne = Value("-1")
  val Laugh = Value("laugh")
  val Confused = Value("confused")
  val Heart = Value("heart")
  val Hooray = Value("hooray")
}
