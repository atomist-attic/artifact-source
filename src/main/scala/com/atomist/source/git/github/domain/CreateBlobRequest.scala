package com.atomist.source.git.github.domain

import org.apache.commons.lang3.StringUtils

private[github] case class CreateBlobRequest(content: String, encoding: String = "base64") {

  override def toString = s"CreateBlobRequest(${StringUtils.abbreviate(content, 20)},$encoding)"
}