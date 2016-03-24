package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.Errors._
import com.github.rbobin.playjsonmatch.processors.{AnyValueProcessor, BooleanProcessor, MissingValueProcessor, NullProcessor}
import com.github.rbobin.playjsonmatch.utils.StringUtils._
import com.github.rbobin.playjsonmatch.utils.{JsMatchException, StringUtils}
import play.api.libs.json.JsValue

import scala.util.matching.Regex

sealed trait MatchAttempt
sealed trait MatchResult extends MatchAttempt {
  def processorName: String
}
case object MatchSkip extends MatchAttempt
case class MatchSuccess(override val processorName: String) extends MatchResult
case class MatchError(override val processorName: String, message: String) extends MatchResult

trait PatternProcessor {
  val regex: Regex

  val rationalNumberRegex = "(-?\\d*\\.{0,1}\\d+)"

  def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt

  val processorName = this.getClass.getSimpleName
  def fail(message: String) = MatchError(processorName, message)
  def success = MatchSuccess(processorName)
  def skip = MatchSkip
}

private[playjsonmatch] object Matcher {
  val defaultProcessors: Seq[PatternProcessor] = Seq(AnyValueProcessor, MissingValueProcessor, BooleanProcessor,
    NullProcessor)
  val splitCharacter = '|'
  val emptyString = ""

  def processPatterns(patterns: String, maybeJsValue: Option[JsValue], path: JsPath): Errors =
    try {
      splitPatterns(patterns)
        .flatMap(processPattern(_, maybeJsValue))
        .flatMap {
          case x: MatchResult => Some(x)
          case _ => None
        }
      match {
        case Nil => throw new JsMatchException(FailureMessages("noMatch"))
        case (x: MatchSuccess) :: Nil => NO_ERRORS
        case (x: MatchError) :: Nil => matchErrors(Seq(x), maybeJsValue, path)
        case xs: Seq[MatchResult] => throw new JsMatchException(FailureMessages("multipleMatch", patterns, xs.map(_.processorName).mkString(", ")))
      }
    } catch {
      case e: JsMatchException =>
        throw new JsMatchException(FailureMessages("errorAtPath", e.message, prettifyPath(path)))
    }

  private def splitPatterns(patterns: String): List[String] =
    patterns.split(splitCharacter)
      .toList
      .filterNot(_.isEmpty)
    match {
      case Nil => throw new JsMatchException(FailureMessages("noPatterns"))
      case x => x
    }

  private def processPattern(pattern: String, maybeJsValue: Option[JsValue]): Seq[MatchAttempt] =
    defaultProcessors.map(processor => processor.process(pattern, maybeJsValue))
}
