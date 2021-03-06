package com.github.rbobin.playjsonmatch

import play.api.libs.json._
import com.github.rbobin.playjsonmatch.utils.StringUtils._

private[playjsonmatch] object Errors {

  def missingElementError(expectedClassName: String, path: JsPath): Errors = ???

  def typeMismatchError(expectedClassName: String, actual: JsValue, path: JsPath): Errors = ???

  def arraysDifferentSizeError(expected: JsArray, actual: JsArray, path: JsPath): Errors = ???

  def objectSupersetError(keys: Seq[String], path: JsPath): Errors = keys.headOption.map { _ =>
    Seq(s"Object ${prettifyPath(path)} does not contain required properties: [${keys.mkString(", ")}]")
  }.getOrElse(Nil)

  def equalityError(expectedClassName: String, expectedValue: String, actualValue: String, path: JsPath): Errors = ???

  def matchErrors(matchErrors: Seq[MatchError], maybeActual: Option[JsValue], path: JsPath): Errors = ???
}
