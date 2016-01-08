package com.github.rbobin.playjsonmatch.utils

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

object Utils {

  def getJsClassName(jsValue: JsValue): String = jsValue match {
    case _: JsArray => ARRAY
    case _: JsObject => OBJECT
    case _: JsString => STRING
    case _: JsNumber => NUMBER
    case _: JsBoolean => BOOLEAN
    case JsNull => NULL
  }

  def getStringRepresentation(maybeJsValue: Option[JsValue]): String = maybeJsValue match {
    case None => NONE
    case Some(x: JsArray) => ARRAY
    case Some(x: JsObject) => OBJECT
    case Some(x: JsString) => s"$STRING: ${x.value}"
    case Some(x: JsNumber) => s"$NUMBER: ${x.value}"
    case Some(x: JsBoolean) => s"$BOOLEAN: ${x.value}"
    case Some(JsNull) => NULL
  }
}
