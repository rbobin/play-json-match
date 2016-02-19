package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.MalformedJsonPatternException
import play.api.libs.json.{JsNumber, JsValue}

object SimpleNumberProcessor extends SimpleProcessor {
  override val regex = "^number$".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsNumber) => success
      case x => fail(FailureMessages("wasNotNumber", x))
    }
}

object NumberInRangeProcessor extends TwoCapturingGroupsProcessor {
  override val regex = s"^number:$rationalNumberRegex:$rationalNumberRegex$$".r

  override def doMatch(min: String, max: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if validateNumber(jsNumber, min, max) => success
      case Some(jsNumber: JsNumber) => fail(FailureMessages("numberRangeMismatch", min, max, jsNumber.value))
      case x => fail(FailureMessages("wasNotNumber", x))
    }

  private def validateNumber(jsNumber: JsNumber, minString: String, maxString: String): Boolean = {
    val min = BigDecimal.exact(minString)
    val max = BigDecimal.exact(maxString)
    if (min.>(max))
      throw new MalformedJsonPatternException(FailureMessages("minValueGreaterThanMaxValue", max, min))

    val number = jsNumber.value
    number >= min && number <= max
  }
}

object LowerBoundedNumberProcessor extends SingleCapturingGroupProcessor {
  override val regex = s"^number:$rationalNumberRegex:$$".r

  override def doMatch(min: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if jsNumber.value >= BigDecimal.exact(min) => success
      case Some(jsNumber: JsNumber) => fail(FailureMessages("numberLowBoundMismatch", jsNumber.value, min))
      case x => fail(FailureMessages("wasNotNumber", x))
    }
}

object UpperBoundedNumberProcessor extends SingleCapturingGroupProcessor {
  override val regex = s"^number::$rationalNumberRegex$$".r

  override def doMatch(max: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if jsNumber.value <= BigDecimal.exact(max) => success
      case Some(jsNumber: JsNumber) => fail(FailureMessages("numberHighBoundMismatch", jsNumber.value, max))
      case x => fail(FailureMessages("wasNotNumber", x))
    }
}
