package com.atomist.source.git.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class Links(self: LinksHref, html: LinksHref, @JsonProperty("pull_request") pullRequest: LinksHref)