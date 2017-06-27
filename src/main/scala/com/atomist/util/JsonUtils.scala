package com.atomist.util

import java.io.InputStream
import java.text.SimpleDateFormat

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper, SerializationFeature}
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

/**
  * Serialize objects to Json.
  */
object JsonUtils {

  private val Mapper = new ObjectMapper() with ScalaObjectMapper
  Mapper.registerModule(DefaultScalaModule)
    .registerModule(new JavaTimeModule())
    .registerModule(new Jdk8Module())
    .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, SerializationFeature.INDENT_OUTPUT)
    .disable(DeserializationFeature.ADJUST_DATES_TO_CONTEXT_TIME_ZONE, DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
    .setSerializationInclusion(Include.NON_NULL)
    .setSerializationInclusion(Include.NON_ABSENT)
    .setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX"))

  def toJson(value: Any): Array[Byte] = Mapper.writeValueAsBytes(value)

  def fromJson[T](is: InputStream)(implicit m: Manifest[T]): T = Mapper.readValue[T](is)
}
