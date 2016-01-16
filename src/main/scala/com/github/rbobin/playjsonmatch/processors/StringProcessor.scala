package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import com.github.rbobin.playjsonmatch.{MatchAttempt, PatternProcessor}
import play.api.libs.json.{JsString, JsValue}

object SimpleStringProcessor extends PatternProcessor {
  val patternString = "^string$"

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case `patternString` => maybeJsValue match {
        case Some(_: JsString) => success
        case x => fail("Any string", x)
      }
      case _ => skip
    }
}

object SizedStringProcessor extends PatternProcessor {
  val pattern = "^string:(\\d+)$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(expectedLength) =>
        maybeJsValue match {
          case Some(jsString: JsString) if jsString.value.length == expectedLength.toInt => success
          case Some(jsString: JsString) =>
            fail(expectedString(expectedLength), s"String of length ${jsString.value.length}")
          case x => fail(expectedString(expectedLength), x)
        }
      case _ => skip
    }

  private def expectedString(expectedSize: String) = s"String of length $expectedSize"
}

object BoundedStringProcessor extends PatternProcessor {
  val pattern = "^string:(\\d+):(\\d+)$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(minLength, maxLength) =>
        maybeJsValue match {
          case Some(jsString: JsString) if checkStringBoundaries(jsString, minLength, maxLength) => success
          case Some(jsString: JsString) =>
            fail(expectedString(minLength, maxLength), s"String of length ${jsString.value.length}")
          case x => fail(expectedString(minLength, maxLength), x)
        }
    }

  private def checkStringBoundaries(jsString: JsString, minLengthString: String, maxLengthString: String): Boolean = {
    val minLength = minLengthString.toInt
    val maxLength = maxLengthString.toInt
    if (minLength > maxLength)
      throw new MalformedJsPatternException("Min length must be not greater than max length")

    val stringLength = jsString.value.length
    stringLength >= minLength && stringLength <= maxLength
  }

  private def expectedString(minLength: String, maxLength: String) = s"String of length $minLength to $maxLength"
}

object LowerBoundedStringProcessor extends PatternProcessor {
  val pattern = "^string:(\\d+):$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(minLength) =>
        maybeJsValue match {
          case Some(jsString: JsString) if jsString.value.length >= minLength.toInt => success
          case Some(jsString: JsString) => fail(expectedString(minLength), s"String of length ${jsString.value.length}")
          case x => fail(expectedString(minLength), x)
        }
    }

  private def expectedString(minLength: String) = s"String of length at least $minLength"
}

object UpperBoundedStringProcessor extends PatternProcessor {
  val pattern = "^string::(\\d+)$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(maxLength) =>
        maybeJsValue match {
          case Some(jsString: JsString) if jsString.value.length <= maxLength.toInt => success
          case Some(jsString: JsString) => fail(expectedString(maxLength), s"String of length ${jsString.value.length}")
          case x => fail(expectedString(maxLength), x)
        }
    }

  private def expectedString(maxLength: String) = s"String of length up to $maxLength"
}