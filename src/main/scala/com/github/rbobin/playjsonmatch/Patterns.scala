package com.github.rbobin.playjsonmatch

import play.api.libs.json.JsValue

import scala.util.matching.Regex


object Matcher {
  val patterns: Seq[Pattern] = Seq(StringPattern)

  def processPattern(pattern: String, jsValue: JsValue): Seq[String] = {
    val patternsMap: Map[String, Option[Pattern]] = pattern
      .split('|')
      .map { patternPiece =>
        (patternPiece, patterns.find(p => p.check(patternPiece)))
      }
      .toMap

    val unmatchedStrings = patternsMap.toSeq.filter(tuple => tuple._2.isEmpty ).map(tuple => tuple._1)

    if (unmatchedStrings.nonEmpty) return unmatchedStrings.map ( string => s"Invalid pattern: $string")

    val matchResults = patternsMap.toSeq.map( tuple => tuple._2.get.tryMatch(tuple._1, jsValue))

    if (matchResults.exists(_.isEmpty)) return Nil

    matchResults.filter(_.nonEmpty).map(errorMessage => s"Pattern did not matched: ${errorMessage.get}")
  }
}

trait Pattern {
  def pattern: Regex

  def check(pattern: String): Boolean

  def tryMatch(pattern: String, jsValue: JsValue): Option[String]
}

object StringPattern extends Pattern {
  val pattern = "something".r

  override def check(pattern: String): Boolean = ???

  override def tryMatch(pattern: String, jsValue: JsValue): Option[String] = ???
}
