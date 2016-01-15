package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{MatchAttempt, PatternProcessor}
import play.api.libs.json.{JsBoolean, JsValue}

object BooleanProcessor extends PatternProcessor {
  val patternString = "boolean"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case `patternString` =>
        maybeJsValue match {
          case Some(x: JsBoolean) => success
          case x => fail("Any boolean", x)
        }
      case _ => skip
    }
}
