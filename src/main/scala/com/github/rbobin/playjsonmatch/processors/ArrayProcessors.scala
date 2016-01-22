package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import play.api.libs.json.{JsArray, JsValue}

object SimpleArrayProcessor extends SimpleProcessor {
  override val regex = "array".r

  override def doMatch(maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(_: JsArray) => success
      case x => fail("Any array", x)
    }
}

object SizedArrayProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^array:(\\d+)$".r

  override def doMatch(expectedSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if jsArray.value.size == expectedSize.toInt => success
      case Some(jsArray: JsArray) => fail(s"Array of size $expectedSize", s"Array of size ${jsArray.value.size}")
      case x => fail(s"Array of size $expectedSize", x)
    }
}

object BoundedArrayProcessor extends TwoCapturingGroupsProcessor {
  override val regex = "^array:(\\d+):(\\d+)$".r

  override def doMatch(minSize: String, maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if validateArraySize(jsArray, minSize.toInt, maxSize.toInt) => success
      case Some(jsArray: JsArray) => fail(s"Array of size $minSize to $maxSize", s"Array of size ${jsArray.value.size}")
      case x => fail(s"Array of size $minSize to $maxSize", x)
    }

  private def validateArraySize(jsArray: JsArray, minSize: Int, maxSize: Int): Boolean = {
    if (minSize > maxSize)
      throw new MalformedJsPatternException(s"Min size ($minSize) must be not greater than min size ($maxSize)")

    val arraySize = jsArray.value.size
    arraySize >= minSize && arraySize <= maxSize
  }
}

object LowerBoundedArrayProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^array:(\\d+):$".r

  override def doMatch(minSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if jsArray.value.size >= minSize.toInt => success
      case Some(jsArray: JsArray) => fail(s"Array of size at least $minSize", s"Array of size ${jsArray.value.size}")
      case x => fail(s"Array of size at least $minSize", x)
    }
}

object UpperBoundedArrayProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^array::(\\d+)$".r

  override def doMatch(maxSize: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsArray: JsArray) if jsArray.value.size <= maxSize.toInt => success
      case Some(jsArray: JsArray) => fail(s"Array of size at most $maxSize", s"Array of size ${jsArray.value.size}")
      case x => fail(s"Array of size at most $maxSize", x)
    }
}
