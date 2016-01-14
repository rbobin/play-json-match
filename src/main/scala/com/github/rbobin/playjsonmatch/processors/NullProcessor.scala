package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json.{JsNull, JsValue}

object NullProcessor extends PatternProcessor {
  val patternString = "null"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case `patternString` => maybeJsValue match {
        case Some(JsNull) => success
        case x => fail(NULL, x)
      }
      case _ => skip
    }
}
