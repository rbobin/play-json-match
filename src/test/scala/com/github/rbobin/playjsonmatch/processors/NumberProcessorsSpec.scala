package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

class NumberProcessorsSpec extends ProcessorSpec{

  override val processor = SimpleNumberProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      ("number:", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsString" in {
    assertAllMatchSuccess(
      (regexString, Some(JsNumber(0))),
      (regexString, Some(JsNumber(1))),
      (regexString, Some(JsNumber(-1))),
      (regexString, Some(JsNumber(BigDecimal(5,5)))),
      (regexString, Some(JsNumber(Integer.MIN_VALUE)))
    )
  }

  it should "fail with relevant pattern and anything but JsString" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsBoolean(false))),
      (regexString, Some(JsString("!"))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}
