package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import play.api.libs.json.{JsString, JsValue}

object SimpleStringProcessor extends SimpleProcessor {
  override val regex = "^string$".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsString) => success
      case x => fail("Any string", x)
    }
}

object SizedStringProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^string:(\\d+)$".r

  override def doMatch(expectedLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if jsString.value.length == expectedLength.toInt => success
      case Some(jsString: JsString) =>
        fail(expectedString(expectedLength), s"String of length ${jsString.value.length}")
      case x => fail(expectedString(expectedLength), x)
    }

  private def expectedString(expectedSize: String) = s"String of length $expectedSize"
}

object BoundedStringProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^string:(\\d+):(\\d+)$".r

  override def doMatch(minLength: String, maxLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if validateStringLength(jsString, minLength.toInt, maxLength.toInt) => success
      case Some(jsString: JsString) =>
        fail(expectedString(minLength, maxLength), s"String of length ${jsString.value.length}")
      case x => fail(expectedString(minLength, maxLength), x)
    }

  private def validateStringLength(jsString: JsString, minLength: Int, maxLength: Int): Boolean = {
    if (minLength > maxLength)
      throw new MalformedJsPatternException(s"Min length ($minLength) must be not greater than max length ($maxLength)")

    val stringLength = jsString.value.length
    stringLength >= minLength && stringLength <= maxLength
  }

  private def expectedString(minLength: String, maxLength: String) = s"String of length $minLength to $maxLength"
}

object LowerBoundedStringProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^string:(\\d+):$".r

  override def doMatch(minLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if jsString.value.length >= minLength.toInt => success
      case Some(jsString: JsString) => fail(expectedString(minLength), s"String of length ${jsString.value.length}")
      case x => fail(expectedString(minLength), x)
    }

  private def expectedString(minLength: String) = s"String of length at least $minLength"
}

object UpperBoundedStringProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^string::(\\d+)$".r

  override def doMatch(maxLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if jsString.value.length <= maxLength.toInt => success
      case Some(jsString: JsString) => fail(expectedString(maxLength), s"String of length ${jsString.value.length}")
      case x => fail(expectedString(maxLength), x)
    }

  private def expectedString(maxLength: String) = s"String of length up to $maxLength"
}