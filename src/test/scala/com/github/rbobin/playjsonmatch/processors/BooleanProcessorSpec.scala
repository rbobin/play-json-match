package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

class BooleanProcessorSpec extends ProcessorSpec {

  override val processor = BooleanProcessor
  val pattern = "boolean"

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
      (pattern, Some(JsBoolean(true))),
      (pattern, Some(JsBoolean(false)))
    )
  }

  it should "fail with relevant pattern and anything else" in {
    assertAllMatchError(
      (pattern, None),
      (pattern, Some(JsNumber(1))),
      (pattern, Some(JsString("..."))),
      (pattern, Some(JsArray(Seq()))),
      (pattern, Some(JsObject(Seq()))),
      (pattern, Some(JsNull))
    )
  }
}
