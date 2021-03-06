package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json.{JsString, JsValue}

object SimpleStringProcessor extends SimpleProcessor {
  override val regex = "^string$".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsString) => success
      case x => fail(FailureMessages("wasNotString", x))
    }
}

object SizedStringProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^string:(\\d+)$".r

  override def doMatch(expectedLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if jsString.value.length == expectedLength.toInt => success
      case Some(jsString: JsString) => fail(FailureMessages("stringSizeMismatch", expectedLength, jsString.value))
      case x => fail(FailureMessages("wasNotString", x))
    }
}

object BoundedStringProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^string:(\\d+):(\\d+)$".r

  override def doMatch(minLength: String, maxLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if validateStringLength(jsString, minLength.toInt, maxLength.toInt) => success
      case Some(jsString: JsString) => fail(FailureMessages("stringBoundariesMismatch", minLength, maxLength, jsString.value))
      case x => fail(FailureMessages("wasNotString", x))
    }

  private def validateStringLength(jsString: JsString, minLength: Int, maxLength: Int): Boolean = {
    if (minLength > maxLength)
      throw new JsMatchException(FailureMessages("minLengthGreaterThanMaxLength", minLength, maxLength))

    val stringLength = jsString.value.length
    stringLength >= minLength && stringLength <= maxLength
  }
}

object LowerBoundedStringProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^string:(\\d+):$".r

  override def doMatch(minLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if jsString.value.length >= minLength.toInt => success
      case Some(jsString: JsString) => fail(FailureMessages("stringMinSizeMismatch", minLength, jsString.value.length))
      case x => fail(FailureMessages("wasNotString", x))
    }
}

object UpperBoundedStringProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^string::(\\d+)$".r

  override def doMatch(maxLength: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) if jsString.value.length <= maxLength.toInt => success
      case Some(jsString: JsString) => fail(FailureMessages("stringMaxSizeMismatch", maxLength, jsString.value.length))
      case x => fail(FailureMessages("wasNotString", x))
    }
}