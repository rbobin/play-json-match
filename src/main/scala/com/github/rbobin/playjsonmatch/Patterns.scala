package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.Errors._
import com.github.rbobin.playjsonmatch.processors._
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
    NullProcessor, RegexProcessor, DateTimeProcessor, BoundedArrayProcessor, LowerBoundedArrayProcessor,
    SimpleArrayProcessor, SizedArrayProcessor, UpperBoundedArrayProcessor, LowerBoundedNumberProcessor,
    NumberInRangeProcessor, SimpleNumberProcessor, UpperBoundedNumberProcessor, BoundedObjectProcessor,
    LowerBoundedObjectProcessor, SimpleObjectProcessor, SizedObjectProcessor, UpperBoundedObjectProcessor,
    BoundedStringProcessor, LowerBoundedStringProcessor, SimpleStringProcessor, SizedStringProcessor,
    UpperBoundedStringProcessor)
  val splitCharacter = '|'
  val emptyString = ""
  val emptyErrors = Left(Nil)

  type ErrorsOrSuccess = Either[List[MatchError], Unit]

  def processPatterns(patterns: String, maybeJsValue: Option[JsValue], path: JsPath): Errors =
    try {
      splitPatterns(patterns)
        .map(processPattern(_, maybeJsValue))
        .foldLeft[ErrorsOrSuccess](emptyErrors)(mergeMatchResults)
      match {
        case Left(errors) => matchErrors(errors, maybeJsValue, path)
        case Right(_) => NO_ERRORS
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

  private def mergeMatchResults(errorsEitherSuccess: ErrorsOrSuccess, matchResult: MatchResult): ErrorsOrSuccess =
    errorsEitherSuccess match {
      case Left(errors) => matchResult match {
        case _: MatchSuccess => Right()
        case error: MatchError => Left(error :: errors)
      }
      case Right(_) => Right()
    }

  private def processPattern(pattern: String, maybeJsValue: Option[JsValue]): MatchResult =
    filterSkipped(pattern, defaultProcessors.map(processor => processor.process(pattern, maybeJsValue)))

  private def filterSkipped(pattern: String, results: Seq[MatchAttempt]): MatchResult =
    results.filterNot(_ == MatchSkip) match {
      case Nil => throw new JsMatchException(FailureMessages("noMatch", pattern))
      case (x: MatchResult) :: Nil => x
      case x: Seq[MatchResult] =>
        throw new JsMatchException(FailureMessages("multipleMatch", pattern, x.map(_.processorName).mkString(",")))
    }
}
