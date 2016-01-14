package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{MatchAttempt, PatternProcessor}
import play.api.libs.json.{JsBoolean, JsValue}

object BooleanProcessor extends PatternProcessor {
  val patternString = "true|false"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case `patternString` =>
        val expected = patternCandidate.toBoolean
        maybeJsValue match {
          case Some(x: JsBoolean) if expected == x.value => success
          case x => fail(s"Boolean [$expected]", x)
        }
      case _ => skip
    }
}
