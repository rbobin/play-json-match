package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json.JsValue

object MissingValueProcessor extends PatternProcessor {
  val patternString = "?"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case `patternString` => maybeJsValue match {
        case None => success
        case x => fail(NONE, x)
      }
      case _ => skip
    }
}
