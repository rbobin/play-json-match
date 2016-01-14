package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.PatternProcessor
import play.api.libs.json.JsValue

object AnyValueProcessor extends PatternProcessor {
  val patternString = "*"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]) =
    patternCandidate match {
      case `patternString` => maybeJsValue match {
        case Some(_) => success
        case x => fail("Anything", x)
      }
      case _ => skip
    }
}
