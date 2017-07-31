package com.atomist.util

/**
  * Helper for HTTP method calls offering JSON marshaling support.
  */
trait RestGateway {

  import HttpMethod._

  /**
    * Make a HTTP request, marshaling the payload and returning JSON if necessary.
    *
    * @param url the path of the request
    * @param method the Http method
    * @param body the body of the request as a byte array
    * @param params a map of request parameters
    * @param headers a map of request headers
    * @tparam T T return type. Should be Unit if we don't return anything
    * @return the unmarshalled object to return
    */
  def httpRequest[T](url: String,
                     method: HttpMethod = Get,
                     body: Option[Array[Byte]] = None,
                     params: Map[String, String] = Map.empty,
                     headers: Map[String, String] = Map.empty)(implicit m: Manifest[T]): T
}

object HttpMethod extends Enumeration {

  type HttpMethod = Value

  val Head = Value("HEAD")
  val Get = Value("GET")
  val Post = Value("POST")
  val Patch = Value("PATCH")
  val Put = Value("PUT")
  val Delete = Value("DELETE")
}
