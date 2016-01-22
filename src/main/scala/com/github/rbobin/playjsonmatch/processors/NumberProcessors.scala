package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import play.api.libs.json.{JsNumber, JsValue}

object SimpleNumberProcessor extends SimpleProcessor {
  override val regex = "^number$".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsNumber) => success
      case x => fail("Any Number", x)
    }
}

object NumberInRangeProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^number:(\\d+):(\\d+)$".r

  override def doMatch(min: String, max: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if validateNumber(jsNumber, min, max) => success
      case x => fail(s"Number in range [$min, $max]", x)
    }

  private def validateNumber(jsNumber: JsNumber, minString: String, maxString: String): Boolean = {
    val min = minString.toInt
    val max = maxString.toInt
    if (min > max)
      throw new MalformedJsPatternException(s"Min ($minString) must be not greater than max ($maxString)")

    val number = jsNumber.value
    number >= min && number <= max
  }
}

object LowerBoundedNumber extends SingleCapturingGroupProcessor {
  override val regex = "^number:(\\d+):$".r

  override def doMatch(min: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if jsNumber.value >= min.toInt => success
      case x => fail(s"Number equals or greater than $min", x)
    }
}

object UpperBoundedNumber extends SingleCapturingGroupProcessor {
  override val regex = "^number::(\\d+)$".r

  override def doMatch(max: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsNumber: JsNumber) if jsNumber.value <= max.toInt => success
      case x => fail(s"Number equals or less than $max", x)
    }
}
