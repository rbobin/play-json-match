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
      case pattern(expectedSize) =>
        maybeJsValue match {
          case Some(jsString: JsString) if jsString.value.length == expectedSize.toInt => success
          case Some(jsString: JsString) => ??? // TODO Custom error
          case x => fail(s"String of size $expectedSize", x)
        }
      case _ => skip
    }
}

object BoundedStringProcessor extends PatternProcessor {
  val pattern = "^string:(\\d+):(\\d+)$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(minSize, maxSize) =>
        maybeJsValue match {
          case Some(jsString: JsString) if checkStringBoundaries(jsString, minSize, maxSize) => success
          case Some(jsString: JsString) => ??? // TODO Custom error
          case x => fail(s"String of size $minSize to $maxSize", x)
        }
    }

  private def checkStringBoundaries(jsString: JsString, minSizeString: String, maxSizeString: String): Boolean = {
    val minSize = minSizeString.toInt
    val maxSize = maxSizeString.toInt
    if (minSize > maxSize)
      throw new MalformedJsPatternException("") // FIXME

    val stringLength = jsString.value.length
    stringLength >= minSize && stringLength <= maxSize
  }
}

object LowerBoundedStringProcessor extends PatternProcessor {
  val pattern = "^string:(\\d+):$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(minSize) =>
        maybeJsValue match {
          case Some(jsString: JsString) if jsString.value.length >= minSize.toInt => success
          case Some(jsString: JsString) => ??? // TODO Custom error
          case x => fail(s"String of size at least $minSize", x)
        }
    }
}

object UpperBoundedStringProcessor extends PatternProcessor {
  val pattern = "^string::(\\d+)$".r

  override def process(patternCandidate: String, maybeJsValue: Option[JsValue]): MatchAttempt =
    patternCandidate match {
      case pattern(maxSize) =>
        maybeJsValue match {
          case Some(jsString: JsString) if jsString.value.length <= maxSize.toInt => success
          case Some(jsString: JsString) => ??? // TODO Custom error
          case x => fail(s"String of size up to $maxSize", x)
        }
    }
}