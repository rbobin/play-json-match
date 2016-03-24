package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json.{JsArray, JsValue}

object SimpleArrayProcessor extends SimpleProcessor {
  override val regex = "array".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsArray) => success
      case x => fail(FailureMessages("wasNotArray", x))
    }
}

object SizedArrayProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^array:(\\d+)$".r

  override def doMatch(expectedSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if jsArray.value.size == expectedSize.toInt => success
      case Some(jsArray: JsArray) => fail(FailureMessages("arraySizeMismatch", expectedSize, jsArray.value.size))
      case x => fail(FailureMessages("wasNotArray", x))
    }
}

object BoundedArrayProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^array:(\\d+):(\\d+)$".r

  override def doMatch(minSize: String, maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if validateArraySize(jsArray, minSize.toInt, maxSize.toInt) => success
      case Some(jsArray: JsArray) => fail(FailureMessages("arrayBoundariesMismatch", minSize, maxSize, jsArray.value.size))
      case x => fail(FailureMessages("wasNotArray", x))
    }

  private def validateArraySize(jsArray: JsArray, minSize: Int, maxSize: Int): Boolean = {
    if (minSize > maxSize)
      throw new JsMatchException(FailureMessages("minSizeGreaterThanMaxSize", minSize, maxSize))

    val arraySize = jsArray.value.size
    arraySize >= minSize && arraySize <= maxSize
  }
}

object LowerBoundedArrayProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^array:(\\d+):$".r

  override def doMatch(minSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if jsArray.value.size >= minSize.toInt => success
      case Some(jsArray: JsArray) => fail(FailureMessages("arrayMinSizeMismatch", minSize, jsArray.value.size))
      case x => fail(FailureMessages("wasNotArray", x))
    }
}

object UpperBoundedArrayProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^array::(\\d+)$".r

  override def doMatch(maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if jsArray.value.size <= maxSize.toInt => success
      case Some(jsArray: JsArray) => fail(FailureMessages("arrayMaxSizeMismatch", maxSize, jsArray.value.size))
      case x => fail(FailureMessages("wasNotArray", x))
    }
}
