package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json.{JsNull, JsValue}

object NullProcessor extends SimpleProcessor {
  override val regex = "null".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(JsNull) => success
      case x => fail(FailureMessages("wasNotNull", x))
    }
}
