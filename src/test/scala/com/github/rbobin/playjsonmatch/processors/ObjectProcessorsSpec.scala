package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

class ObjectProcessorsSpec extends ProcessorSpec {

  override val processor = SimpleObjectProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      (" object ", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsObject" in {
    assertAllMatchSuccess(
      (regexString, Some(JsObject(Seq()))),
      (regexString, Some(JsObject(Seq(("field", JsNull))))),
      (regexString, Some(JsObject(Seq(("1", JsNumber(1)), ("2", JsBoolean(false))))))
    )
  }

  it should "fail with relevant pattern and anything but JsObject" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsNumber(1000))),
      (regexString, Some(JsString("!"))),
      (regexString, Some(JsBoolean(true))),
      (regexString, Some(JsArray(Seq())))
    )
  }
}
