package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import play.api.libs.json.{JsObject, JsValue}

object SimpleObjectProcessor extends SimpleProcessor {
  override val regex = "^object$".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsObject) => success
      case x => fail("Any object", x)
    }
}

object SizedObjectProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^object:(\\d+)$".r

  override def doMatch(expectedSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if jsObject.values.size == expectedSize.toInt => success
      case Some(jsObject: JsObject) => fail(s"Object of size $expectedSize", s"Object of size ${jsObject.values.size}")
      case x => fail(s"Object of size $expectedSize", x)
    }
}

object BoundedObjectProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^object:(\\d+):(\\d+)$".r

  override def doMatch(minSize: String, maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if validateObjectSize(jsObject, minSize.toInt, maxSize.toInt) => success
      case Some(jsObject: JsObject) =>
        fail(s"Object of size from $minSize to $maxSize", s"Object of size ${jsObject.values.size}")
      case x => fail(s"Object of size from $minSize to $maxSize", x)
    }

  private def validateObjectSize(jsObject: JsObject, minSize: Int, maxSize: Int) = {
    if (minSize > maxSize)
      throw new MalformedJsPatternException(s"Min size ($minSize) must be not greater than min size ($maxSize)")

    val objectSize = jsObject.values.size
    objectSize >= minSize && objectSize <= maxSize
  }
}

object LowerBoundedProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^object:(\\d+):$".r

  override def doMatch(minSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if jsObject.values.size >= minSize.toInt => success
      case Some(jsObject: JsObject) =>
        fail(s"Object of size at least $minSize", s"Object of size ${jsObject.values.size}")
      case x => fail(s"Object of size at least $minSize", x)
    }
}

object UpperBoundedProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^object::(\\d+)$".r

  override def doMatch(maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsObject: JsObject) if jsObject.values.size <= maxSize.toInt => success
      case Some(jsObject: JsObject) =>
        fail(s"Object of size at most $maxSize", s"Object of size ${jsObject.values.size}")
      case x => fail(s"Object of size at most $maxSize", x)
    }
}
