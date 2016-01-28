package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json.JsValue

object MissingValueProcessor extends SimpleProcessor {
  override val regex = "\\?".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case None => success
      case x => fail(FailureMessages("wasNotMissing"))
    }
}
