package com.github.rbobin.playjsonmatch.utils

import play.api.libs.json._

trait Prettifier extends (Any => String)

object Prettifier {
  val default: Prettifier =
    new Prettifier {
      override def apply(v1: Any): String = v1 match {
        case null => "null"
        case x: Unit => "<(), the Unit value>"
        case x: String => "\"" + x + "\""
        case JsNull => "Null"
        case JsString(x) => "\"" + x + "\""
        case JsNumber(x) => x.toString
        case JsBoolean(x) => x.toString
        case JsArray(x) => s"Array[${x.length}]"
        case JsObject(x) => s"Object[${x.values.size}]"
      }
    }
}
