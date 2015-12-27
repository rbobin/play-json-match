package com.github.rbobin.playjsonmatch

import play.api.libs.json._

private[playjsonmatch] object Errors {

  def missingElementError(expectedClassName: String, path: JsPath): Errors = ???

  def typeMismatchError(expectedClassName: String, actual: JsValue, path: JsPath): Errors = ???

  def arraysDifferentSizeError(expected: JsArray, actual: JsArray, path: JsPath): Errors = ???

  def objectSupersetError(keys: Seq[String], path: JsPath): Errors = keys.headOption.map { _ =>
    Seq(s"Object ${prettifyPath(path)} does not contain required properties: [${keys.mkString(", ")}]")
  }.getOrElse(Nil)

  def equalityError(expecedClassName: String, expectedValue: String, actualValue: String, path: JsPath): Errors = ???

  private def prettifyPath(path: JsPath): String = path.headOption.map { _ =>
    "/ " + path.mkString(" / ")
  }.getOrElse("/")

  private def getJsClassName(jsValue: JsValue): String = jsValue match {
    case _: JsArray => ARRAY
    case _: JsObject => OBJECT
    case _: JsString => STRING
    case _: JsNumber => NUMBER
    case _: JsBoolean => BOOLEAN
    case JsNull => NULL
  }
}
