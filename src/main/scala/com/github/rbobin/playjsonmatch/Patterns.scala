package com.github.rbobin.playjsonmatch

import play.api.libs.json.JsValue

object Matcher {
  val patterns: Seq[JsonPattern] = Seq(Anything)

  def processPattern(pattern: String, maybeJsValue: Option[JsValue], path: JsPath): Errors = {
    val patternsMap: Map[String, Option[JsonPattern]] = pattern
      .split('|')
      .map { patternPiece =>
        (patternPiece, patterns.find(p => p.check(patternPiece)))
      }
      .toMap

    val unmatchedStrings = patternsMap.toSeq.filter(tuple => tuple._2.isEmpty ).map(tuple => tuple._1)

    if (unmatchedStrings.nonEmpty) return unmatchedStrings.map ( string => s"Invalid pattern: $string")

    val matchResults = patternsMap.toSeq.map( tuple => tuple._2.get.tryMatch(tuple._1, maybeJsValue))

    if (matchResults.exists(_.isEmpty)) return NO_ERRORS

    matchResults.filter(_.nonEmpty).map(errorMessage => s"Pattern did not matched: ${errorMessage.get}")
  }
}

trait JsonPattern {

  def check(pattern: String): Boolean

  def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String]
}

object Anything extends JsonPattern {

  val PATTERN = "*"

  override def check(pattern: String): Boolean = pattern == PATTERN

  override def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String] = maybeJsValue match {
    case None => Some("Expected any element, found none")
    case _ => None
  }
}
