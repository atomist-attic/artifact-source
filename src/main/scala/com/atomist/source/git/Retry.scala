package com.atomist.source.git

import com.atomist.source.ArtifactSourceException
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

/**
  * Generic retry with exponential backoff and jitter.
  */
object Retry extends LazyLogging {

  private val Rnd = Random

  /**
    * Implement retry with exponential backoff and jiggle.  Each subsequent invocation
    * will wait double + jitter of the previous.  Defaults for retries and wait result
    * in a mean retry period of about 50 seconds.
    *
    * @param opName name of operation, used for logging
    * @param n how many attempts to make
    * @param wait amount of time to wait
    * @param fn operation to retry
    * @tparam T return type pf fn
    * @return return value from successful call of fn
    */
  @tailrec
  final def retry[T](opName: String, n: Int = 4, wait: Long = 0L)(fn: => T): T = {
    Thread.sleep(wait)
    Try(fn) match {
      case Success(x) => x
      case Failure(e: DoNotRetryException) => throw ArtifactSourceException(e.getMessage, e)
      case Failure(e) if n > 0 =>
        logger.debug(s"$opName attempt failed (${e.getMessage}), $n attempts left", e)
        retry(opName, n - 1, wait * 2L + Rnd.nextInt(100))(fn)
      case Failure(e) => throw e
    }
  }
}

private[git] case class DoNotRetryException(msg: String, cause: Throwable) extends RuntimeException(msg, cause)

private[git] object DoNotRetryException {

  def apply(msg: String): DoNotRetryException =
    DoNotRetryException(msg, null)
}
