package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class Webhook(id: Int,
                   name: String,
                   config: WebhookConfig,
                   active: Boolean,
                   events: Array[String],
                   url: String,
                   @JsonProperty(value = "test_url") testUrl: String,
                   @JsonProperty(value = "ping_url") pingUrl: String,
                   @JsonProperty(value = "updated_at") updatedAt: OffsetDateTime,
                   @JsonProperty(value = "created_at") createdAt: OffsetDateTime)