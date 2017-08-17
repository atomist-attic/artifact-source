package com.atomist.source.git

object TestConstants {

  val GheApiUrl = "https://ghe.atomist.services/api/v3/"

  val Token: String = System.getenv("GITHUB_TEST_TOKEN")

  val TemporaryRepoPrefix = "TEST_CAN_DELETE_"

  val TestWebHookUrlBase = "http://artifact-source-test.atomist.com/webhook/"

  /**
    * We will create and delete repos here.
    */
  val TestOrg = "atomisthqtest"
}
