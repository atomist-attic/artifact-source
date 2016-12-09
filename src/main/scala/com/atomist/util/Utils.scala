package com.atomist.util

import java.io.File
import java.util.Optional

import scala.language.{implicitConversions, reflectiveCalls}

/**
  * Used for file operations etc.
  */
object Utils {

  /**
    * Implicit conversion from Scala Option to Java 8 option.
    * Useful when we want to expose a method for usage from Java.
    */
  implicit def toOptional[T](dO: Option[T]): Optional[T] = {
    dO match {
      case Some(d) => Optional.of(d)
      case None => Optional.empty()
    }
  }

  implicit def toOption[T](jO: Optional[T]): Option[T] = {
    if (jO.isPresent) Some(jO.get) else None
  }

  /**
    * Implicit conversion from Scala function to Java 8 Function.
    */
  implicit def toFunction[From, To](function: (From) => To): java.util.function.Function[From, To] = {
    new java.util.function.Function[From, To] {
      override def apply(input: From): To = function(input)
    }
  }

  type Closeable = {def close(): Unit}

  /**
    * Use the given closeable to get a result.
    */
  def withCloseable[T <: Closeable, R](closable: T)(block: T => R): R = {
    try {
      block(closable)
    } finally {
      closable.close()
    }
  }

  def withCloseable[T <: Closeable, R](f: Unit => T)(block: T => R): R = {
    var closable: Option[T] = None
    try {
      closable = Some(f())
      block(closable.get)
    } finally {
      if (closable.isDefined)
        closable.get.close()
    }
  }

  def withCloseable[C <: Closeable](f: Unit => C)(block: C => Unit): Unit =
    withCloseable[C, Unit](f)(block)

  def separatorPattern ={
    File.separator match {
      case """\""" => s"""\\${File.separator}"""
      case _ => File.separator
    }
  }
}
