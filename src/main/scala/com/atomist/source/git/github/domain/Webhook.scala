package com.atomist.source.git.github.domain

import java.time.OffsetDateTime
import java.util.{List => JList}

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonProperty.Access

import scala.collection.JavaConverters._

case class Webhook(name: String,
                   config: WebhookConfig,
                   active: Boolean,
                   events: Seq[String],
                   @JsonProperty(access = Access.WRITE_ONLY) id: Int = 0,
                   @JsonProperty(access = Access.WRITE_ONLY) url: String = "",
                   @JsonProperty(value = "test_url", access = Access.WRITE_ONLY) testUrl: String = "",
                   @JsonProperty(value = "ping_url", access = Access.WRITE_ONLY) pingUrl: String = "",
                   @JsonProperty(value = "updated_at", access = Access.WRITE_ONLY) updatedAt: OffsetDateTime = OffsetDateTime.now,
                   @JsonProperty(value = "created_at", access = Access.WRITE_ONLY) createdAt: OffsetDateTime = OffsetDateTime.now) {
}

object Webhook {

  def apply(name: String, url: String, contentType: String, events: Seq[String]) =
    new Webhook(name, WebhookConfig(url, contentType), active = true, events)

  def apply(name: String, url: String, contentType: String, events: JList[String]) =
    new Webhook(name, WebhookConfig(url, contentType), active = true, events.asScala)
}
