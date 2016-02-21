package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

class BooleanProcessorSpec extends ProcessorSpec {

  override val processor = BooleanProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("*", maybeJsValue),
      ("?!", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsBoolean" in {
    assertAllMatchSuccess(
      (regexString, Some(JsBoolean(true))),
      (regexString, Some(JsBoolean(false)))
    )
  }

  it should "fail with relevant pattern and anything else" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNumber(1))),
      (regexString, Some(JsString("..."))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq()))),
      (regexString, Some(JsNull))
    )
  }
}
