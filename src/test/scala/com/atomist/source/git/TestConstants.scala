package com.atomist.source.git

import com.atomist.source.SimpleCloudRepoId

object TestConstants {

  val GheApiUrl = "https://ghe.atomist.services/api/v3/"

  val Token = System.getenv("GITHUB_TEST_TOKEN")

  val TemporaryRepoPrefix = "TEST_CAN_DELETE_"

  /**
    * We will create and delete repos here.
    */
  val TestOrg = "atomisthqtest"

  val TestTargetRepo = "test-target"

  val TestTargetRepoInfo = SimpleCloudRepoId(TestTargetRepo, TestOrg)

  val TestTargetExpectedBranch = "expected-branch"
}
