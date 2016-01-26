package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import play.api.libs.json.{JsBoolean, JsValue}

object BooleanProcessor extends SimpleProcessor {
  override val regex = "boolean".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(x: JsBoolean) => success
      case x => fail(FailureMessages("wasNotBoolean", x))
    }
}
