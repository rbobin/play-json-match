package com.github.rbobin.playjsonmatch

import play.api.libs.json.JsArray

private[playjsonmatch] object Errors {

  def missingElementError[A](expected: Class[A], path: JsPath): Errors = ???

  def typeMismatchError[A, B](expected: Class[A], actual: Class[B], path: JsPath): Errors = ???

  def arraysDifferentSizeError(expected: JsArray, actual: JsArray, path: JsPath): Errors = ???

  def objectSupersetError(keys: Seq[String], path: JsPath): Errors = keys.headOption.map { _ =>
    Seq(s"Object ${prettifyPath(path)} does not contain required properties: [${keys.mkString(", ")}]")
  }.getOrElse(Nil)

  def equalityError[A](clazz: Class[A], expectedValue: String, actualValue: String, path: JsPath): Errors = ???

  private def prettifyPath(path: JsPath): String = path.headOption.map { _ =>
    "/ " + path.mkString(" / ")
  }.getOrElse("/")
}
