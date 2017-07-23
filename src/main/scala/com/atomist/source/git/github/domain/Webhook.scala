package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty.Access
import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}

import scala.collection.JavaConverters._

@JsonCreator
case class Webhook(@JsonProperty(access = Access.WRITE_ONLY) id: Int = 0,
                   name: String,
                   config: WebhookConfig,
                   active: Boolean,
                   events: Seq[String],
                   @JsonProperty(access = Access.WRITE_ONLY) url: String = "",
                   @JsonProperty(value = "test_url", access = Access.WRITE_ONLY) testUrl: String = "",
                   @JsonProperty(value = "ping_url", access = Access.WRITE_ONLY) pingUrl: String = "",
                   @JsonProperty(value = "updated_at", access = Access.WRITE_ONLY) updatedAt: OffsetDateTime = OffsetDateTime.now,
                   @JsonProperty(value = "created_at", access = Access.WRITE_ONLY) createdAt: OffsetDateTime = OffsetDateTime.now)
object Webhook {

  def apply(name: String, url: String, contentType: String, events: Seq[String]) =
    new Webhook(name = name, config = WebhookConfig(url, contentType), active = true, events = events)

  def apply(whi: WebhookInfo): Webhook =
    Webhook(whi.id, whi.name, WebhookConfig(whi.url, whi.contentType), active = true, whi.events.asScala)
}
