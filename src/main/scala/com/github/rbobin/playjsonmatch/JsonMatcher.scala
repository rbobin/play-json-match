package com.github.rbobin.playjsonmatch

import org.scalatest.matchers.{MatchResult, Matcher}
import play.api.libs.json.JsValue

trait JsonMatcher {

  class JsonPatternMatcher(jsonPattern: JsValue) extends Matcher[JsValue] {
    override def apply(left: JsValue): org.scalatest.matchers.MatchResult = MatchResult(
      Core.matches(jsonPattern, left),
      "Json does not match pattern",
      "Json does match pattern")
  }

  def matchJsonPattern(jsonPattern: JsValue) = new JsonPatternMatcher(jsonPattern)
}

// Make it easy to import with: import JsonMatcher._
object JsonMatcher extends JsonMatcher
