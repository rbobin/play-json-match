package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{MatchAttempt, PatternProcessor}
import play.api.libs.json.{JsBoolean, JsValue}

import scala.util.matching.Regex

object BooleanProcessor extends SimpleProcessor {
  override val regex = "boolean".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(x: JsBoolean) => success
      case x => fail("Any boolean", x)
    }
}
