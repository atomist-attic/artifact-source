package com.atomist.source

trait CloudRepoId {

  def repo: String

  def owner: String

  override def toString = s"repository '$repo', owner '$owner'"
}

case class SimpleCloudRepoId(repo: String, owner: String) extends CloudRepoId

