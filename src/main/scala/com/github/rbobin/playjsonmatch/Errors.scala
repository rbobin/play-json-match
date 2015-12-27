package com.github.rbobin.playjsonmatch

import play.api.libs.json.JsArray

private[playjsonmatch] object Errors {

  def missingElementError[A](expected: Class[A], path: Seq[String]): Seq[String] = ???

  def typeMismatchError[A, B](expected: Class[A], actual: Class[B], path: Seq[String]): Seq[String] = ???

  def arraysDifferentSizeError(expected: JsArray, actual: JsArray, path: Seq[String]): Seq[String] = ???

  def objectSupersetError(keys: Seq[String], path: Seq[String]): Seq[String] = keys.headOption.map { _ =>
    Seq(s"Object ${prettifyPath(path)} does not contain required properties: [${keys.mkString(", ")}]")
  }.getOrElse(Nil)

  def equalityError[A](clazz: Class[A], expectedValue: String, actualValue: String, path: Seq[String]): Seq[String] = ???

  private def prettifyPath(path: Seq[String]): String = path.headOption.map { _ =>
    "/ " + path.mkString(" / ")
  }.getOrElse("/")
}
