package com.atomist.source.git.github.domain

import java.util.{List => JList}

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonProperty.Access

import scala.collection.JavaConverters._

case class WebhookInfo(@JsonProperty(access = Access.WRITE_ONLY) id: Int,
                       name: String,
                       url: String,
                       contentType: String,
                       events: JList[String]) {

  def this(name: String, url: String, contentType: String, events: JList[String]) =
    this(0, name, url, contentType, events)

  def this(wh: Webhook) =
    this(wh.id, wh.name, wh.config.url, wh.config.contentType, wh.events.asJava)
}

object WebhookInfo {

  def apply(wh: Webhook) = new WebhookInfo(wh)
}