package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.utils.Utils
import play.api.libs.json.{JsNull, JsValue}

object Matcher {
  val patterns: Seq[PatternHolder] = Seq(AnythingHolder, MissingHolder)

  def processPattern(pattern: String, maybeJsValue: Option[JsValue], path: JsPath): Errors = {
    val patternsMap: Map[String, Option[PatternHolder]] = pattern
      .split('|')
      .map { patternPiece =>
        (patternPiece, patterns.find(p => p.isMatch(patternPiece)))
      }
      .toMap

    val unmatchedStrings = patternsMap.toSeq.filter(tuple => tuple._2.isEmpty ).map(tuple => tuple._1)

    if (unmatchedStrings.nonEmpty) return unmatchedStrings.map ( string => s"Invalid pattern: $string")

    val matchResults = patternsMap.toSeq.map( tuple => tuple._2.get.tryMatch(tuple._1, maybeJsValue))

    if (matchResults.exists(_.isEmpty)) return NO_ERRORS

    matchResults.filter(_.nonEmpty).map(errorMessage => s"Pattern did not matched: ${errorMessage.get}")
  }
}

trait PatternHolder {

  def isMatch(pattern: String): Boolean

  def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String]
}

object AnythingHolder extends PatternHolder {

  val PATTERN = "*"

  override def isMatch(pattern: String): Boolean = pattern == PATTERN

  override def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String] = maybeJsValue match {
    case None => Some(s"Expected any element, found: ${Utils.getStringRepresentation(None)}")
    case _ => None
  }
}

object MissingHolder extends PatternHolder {

  val PATTERN = "?"

  override def isMatch(pattern: String): Boolean = pattern == PATTERN

  override def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String] = maybeJsValue match {
    case None => None
    case x => Some(s"Expected missing element, found: ${Utils.getStringRepresentation(x)}")
  }
}

object NullHolder extends PatternHolder {

  val PATTERN = "null"

  override def isMatch(pattern: String): Boolean = pattern == PATTERN

  override def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String] = maybeJsValue match {
    case Some(JsNull) => None
    case x => Some(s"Expected $NULL, found: ${Utils.getStringRepresentation(x)}")
  }
}

object BooleanHolder extends PatternHolder {

  val PATTERN = "true|false"

  override def isMatch(pattern: String): Boolean = pattern.matches(PATTERN)

  override def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String] = {
    if (pattern.matches(PATTERN))
      None
    else
      Some(s"Expected Boolean: $pattern, found: ${Utils.getStringRepresentation(maybeJsValue)})")
  }
}

object StringHolder extends PatternHolder {
  val PATTERN = "^string$|^string:\\d+$|^string:\\d+:$|^string::\\d+$|^string:\\d+:\\d+$"

  override def isMatch(pattern: String): Boolean = ???

  override def tryMatch(pattern: String, maybeJsValue: Option[JsValue]): Option[String] = ???
}
