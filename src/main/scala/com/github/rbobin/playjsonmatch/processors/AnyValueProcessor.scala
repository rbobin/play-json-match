package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import play.api.libs.json.JsValue

object AnyValueProcessor extends SimpleProcessor {
  override val regex = "\\*".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_) => success
      case _ => fail(FailureMessages("wasNotAnything"))
    }
}
