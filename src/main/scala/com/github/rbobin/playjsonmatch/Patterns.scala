package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.utils.Utils
import play.api.libs.json.{JsBoolean, JsNull, JsValue}
import Errors._

object Matcher {
  val processors: Seq[PatternProcessor] = Seq(AnythingProcessor)
  val splitCharacter = "|"

  def processPatterns(patterns: String, maybeJsValue: Option[JsValue], path: JsPath): Errors = {
    val matchResults = patterns
      .split(splitCharacter)
      .map(pattern => processPattern(pattern, maybeJsValue))

    if (matchResults.exists(_.isInstanceOf[MatchSuccess]))
      NO_ERRORS
    else {
      val matchErrors = matchResults.collect { case x: MatchError => x }
      matchError(matchErrors, maybeJsValue, path)
    }
  }

  private def processPattern(pattern: String, maybeJsValue: Option[JsValue]): MatchResult =
    filterSkipped(pattern, processors.map(processor => processor.process(pattern, maybeJsValue)))

  private def filterSkipped(pattern: String, results: Seq[MatchResult]): MatchResult =
    results.filter(_ != MatchSkipped) match {
      case Nil => throw new RuntimeException("Pattern didn't match anything")
      case x :: Nil => x
      case x: Seq[MatchResult] =>
        throw new RuntimeException(s"Multiple matches for single pattern $pattern : ${x.map(_.processorName).mkString(",")}")
    }
}

sealed trait MatchResult {
  def processorName: String
}
case object MatchSkipped extends MatchResult {
  val processorName = "Nothing"
}
case class MatchSuccess(processorName: String) extends MatchResult
case class MatchError(expected: String, processorName: String) extends MatchResult

trait PatternProcessor {
  def processorName = this.getClass.getSimpleName
  def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchResult
  def fail(expected: String, maybeJsValue: Option[JsValue]) =
    MatchError(s"Expected $expected, found ${Utils.getStringRepresentation(maybeJsValue)}", processorName)
  def success = MatchSuccess(processorName)
  def skip = MatchSkipped
  def buildExpected(expected: String, params: String*): String = ???
}

object AnythingProcessor extends PatternProcessor {
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

object MissingProcessor extends PatternProcessor {
  val patternString = "?"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchResult =
    patternCandidate match {
      case `patternString` => maybeJsValue match {
        case None => success
        case x => fail(NONE, x)
      }
      case _ => skip
    }
}

object NullProcessor extends PatternProcessor {
  val patternString = "null"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchResult =
    patternCandidate match {
      case `patternString` => maybeJsValue match {
        case Some(JsNull) => success
        case x => fail(NULL, x)
      }
      case _ => skip
    }
}

object BooleanProcessor extends PatternProcessor {
  val patternString = "true|false"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchResult =
    patternCandidate match {
      case `patternString` =>
        val expected = patternCandidate.toBoolean
        maybeJsValue match {
          case Some(x: JsBoolean) if expected == x.value => success
          case x => fail(s"Boolean [$expected]", x)
        }
      case _ => skip
    }
}
