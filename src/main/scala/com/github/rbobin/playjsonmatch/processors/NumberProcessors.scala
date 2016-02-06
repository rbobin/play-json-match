package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
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
  override val regex = "^number:(-?\\d*\\.{0,1}\\d+):(-?\\d*\\.{0,1}\\d+)$".r

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
      throw new MalformedJsPatternException(FailureMessages("minValueGreaterThanMaxValue", max, min))

    val number = jsNumber.value
    number >= min && number <= max
  }
}

object LowerBoundedNumber extends SingleCapturingGroupProcessor {
  override val regex = "^number:(\\d+):$".r

  override def doMatch(min: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if jsNumber.value >= min.toInt => success
      case Some(jsNumber: JsNumber) => fail(FailureMessages("numberLowBoundMismatch", jsNumber.value, min))
      case x => fail(FailureMessages("wasNotNumber", x))
    }
}

object UpperBoundedNumber extends SingleCapturingGroupProcessor {
  override val regex = "^number::(\\d+)$".r

  override def doMatch(max: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if jsNumber.value <= max.toInt => success
      case Some(jsNumber: JsNumber) => fail(FailureMessages("numberHighBoundMismatch", jsNumber.value, max))
      case x => fail(FailureMessages("wasNotNumber", x))
    }
}
