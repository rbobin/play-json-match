package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{MatchAttempt, MatchResult, PatternProcessor}
import play.api.libs.json.JsValue

trait SimpleProcessor extends PatternProcessor {
  override final def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt = {
    val regexString = regex.regex
    patternCandidate match {
      case `regexString` => doMatch(maybeJsValue)
      case _ => skip
    }
  }

  def doMatch(maybeJsValue: Option[JsValue]): MatchResult
}

trait SingleCapturingGroupProcessor extends PatternProcessor {
  override final def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt = {
    patternCandidate match {
      case regex(m) => doMatch(m, maybeJsValue)
      case _ => skip
    }
  }

  def doMatch(matched: String, maybeJsValue: Option[JsValue]): MatchResult
}

trait TwoCapturingGroupsProcessor extends PatternProcessor {
  override final def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt = {
    patternCandidate match {
      case regex(m1, m2) => doMatch(m1, m2, maybeJsValue)
      case _ => skip
    }
  }

  def doMatch(matched1: String, matched2: String, maybeJsValue: Option[JsValue]): MatchResult
}

trait ThreeCapturingGroupsProcessor extends PatternProcessor {
  override final def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt = {
    patternCandidate match {
      case regex(m1, m2, m3) => doMatch(m1, m2, m3, maybeJsValue)
      case _ => skip
    }
  }

  def doMatch(match1: String, match2: String, match3: String, maybeJsValue: Option[JsValue]): MatchResult
}

trait FourCapturingGroupsProcessor extends PatternProcessor {
  override final def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt = {
    patternCandidate match {
      case regex(m1, m2, m3, m4) => doMatch(m1, m2, m3, m4, maybeJsValue)
      case _ => skip
    }
  }

  def doMatch(match1: String, match2: String, match3: String, match4: String, maybeJsValue: Option[JsValue]): MatchResult
}

trait FiveCapturingGroupsProcessor extends PatternProcessor {
  override final def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt = {
    patternCandidate match {
      case regex(m1, m2, m3, m4, m5) => doMatch(m1, m2, m3, m4, m5, maybeJsValue)
      case _ => skip
    }
  }

  def doMatch(match1: String, match2: String, match3: String, match4: String, match5: String, maybeJsValue: Option[JsValue]): MatchResult
}