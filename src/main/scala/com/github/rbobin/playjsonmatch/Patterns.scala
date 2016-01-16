package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.processors.{NullProcessor, BooleanProcessor, MissingValueProcessor, AnyValueProcessor}
import com.github.rbobin.playjsonmatch.utils.{MultipleMatchException, MalformedJsPatternException, StringUtils}
import play.api.libs.json.JsValue
import Errors._
import StringUtils._

sealed trait MatchAttempt
sealed trait MatchResult extends MatchAttempt {
  def processorName: String
}
case object MatchSkip extends MatchAttempt
case class MatchSuccess(override val processorName: String) extends MatchResult
case class MatchError(override val processorName: String, expected: String, found: String) extends MatchResult

trait PatternProcessor {
  def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt

  val processorName = this.getClass.getSimpleName
  def fail(expected: String, found: String) = MatchError(processorName, expected, found)
  def fail(expected: String, maybeJsValue: Option[JsValue]) =
    MatchError(processorName, expected, StringUtils.toString(maybeJsValue))
  def success = MatchSuccess(processorName)
  def skip = MatchSkip
}

private[playjsonmatch] object Matcher {
  val defaultProcessors: Seq[PatternProcessor] = Seq(AnyValueProcessor, MissingValueProcessor, BooleanProcessor,
    NullProcessor)
  val splitCharacter = '|'
  val emptyString = ""
  val emptyErrors = Left(Nil)

  type ErrorsOrSuccess = Either[List[MatchError], Unit]

  def processPatterns(patterns: String, maybeJsValue: Option[JsValue], path: JsPath): Errors =
    try {
      splitJsPatterns(patterns)
        .map(verifyNotEmpty)
        .map(processPattern(_, maybeJsValue))
        .foldLeft[ErrorsOrSuccess](emptyErrors)(mergeMatchResults)
      match {
        case Left(errors) => matchErrors(errors, maybeJsValue, path)
        case Right(_) => NO_ERRORS
      }
    } catch {
      case e: MultipleMatchException =>
        val newMessage = s"${e.message} at ${prettifyPath(path)}"
        throw new MultipleMatchException(newMessage)
      case e: MalformedJsPatternException =>
        val newMessage = s"${e.message} at ${prettifyPath(path)}"
        throw new MalformedJsPatternException(newMessage)
    }

  private def splitJsPatterns(jsPatterns: String): List[String] =
    jsPatterns.split(splitCharacter).toList match {
      case Nil => throw new MalformedJsPatternException("No patterns found")
      case x => x
    }

  private def verifyNotEmpty(jsPattern: String): String =
    jsPattern match {
      case `emptyString` => throw new MalformedJsPatternException("Empty pattern")
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
      case Nil => throw new MalformedJsPatternException(s"Pattern [ $pattern ] doesn't match anything")
      case (x: MatchResult) :: Nil => x
      case x: Seq[MatchResult] =>
        throw new MultipleMatchException(s"Multiple matches for single pattern [ $pattern ]: ${x.map(_.processorName).mkString(",")}")
    }
}
