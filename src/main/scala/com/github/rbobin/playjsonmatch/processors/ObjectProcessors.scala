package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json.{JsObject, JsValue}

object SimpleObjectProcessor extends SimpleProcessor {
  override val regex = "^object$".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsObject) => success
      case x => fail(FailureMessages("wasNotObject", x))
    }
}

object SizedObjectProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^object:(\\d+)$".r

  override def doMatch(expectedSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if jsObject.values.size == expectedSize.toInt => success
      case Some(jsObject: JsObject) => fail(FailureMessages("objectSizeMismatch", jsObject.values.size))
      case x => fail(FailureMessages("wasNotObject", x))
    }
}

object BoundedObjectProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^object:(\\d+):(\\d+)$".r

  override def doMatch(minSize: String, maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if validateObjectSize(jsObject, minSize.toInt, maxSize.toInt) => success
      case Some(jsObject: JsObject) =>
        fail(FailureMessages("objectBoundariesMismatch", minSize, maxSize, jsObject.values.size))
      case x => fail(FailureMessages("wasNotObject", x))
    }

  private def validateObjectSize(jsObject: JsObject, minSize: Int, maxSize: Int) = {
    if (minSize > maxSize)
      throw new JsMatchException(FailureMessages("minSizeGreaterThanMaxSize", minSize, maxSize))

    val objectSize = jsObject.values.size
    objectSize >= minSize && objectSize <= maxSize
  }
}

object LowerBoundedObjectProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^object:(\\d+):$".r

  override def doMatch(minSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if jsObject.values.size >= minSize.toInt => success
      case Some(jsObject: JsObject) =>
        fail(FailureMessages("objectMinSizeMismatch", minSize, jsObject.values.size))
      case x => fail(FailureMessages("wasNotObject", x))
    }
}

object UpperBoundedObjectProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^object::(\\d+)$".r

  override def doMatch(maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if jsObject.values.size <= maxSize.toInt => success
      case Some(jsObject: JsObject) =>
        fail(FailureMessages("objectMaxSizeMismatch", maxSize, jsObject.values.size))
      case x => fail(FailureMessages("wasNotObject", x))
    }
}
