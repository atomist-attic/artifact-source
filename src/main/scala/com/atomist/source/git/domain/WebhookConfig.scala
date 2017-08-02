package com.atomist.source.git.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class WebhookConfig(url: String,
                         @JsonProperty("content_type") contentType: String = "json")

