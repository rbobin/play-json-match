package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

class ArrayProcessorsSpec extends ProcessorSpec {

  override val processor = SimpleArrayProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      (" array ", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsArray" in {
    assertAllMatchSuccess(
      (regexString, Some(JsArray())),
      (regexString, Some(JsArray(Seq(JsBoolean(true))))),
      (regexString, Some(JsArray(Seq.fill(5)(JsNull))))
    )
  }

  it should "fail with relevant pattern and anything but JsArray" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsNumber(1000))),
      (regexString, Some(JsString("!"))),
      (regexString, Some(JsBoolean(true))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}
