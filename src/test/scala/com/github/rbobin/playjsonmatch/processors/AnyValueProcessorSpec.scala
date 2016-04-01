package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.MatchError
import play.api.libs.json._

class AnyValueProcessorSpec extends ProcessorSpec {

  override val processor = AnyValueProcessor
  val pattern = "*"

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      ("**", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and any value" in {
    assertAllMatchSuccess(
      (pattern, Some(JsNull)),
      (pattern, Some(JsString(""))),
      (pattern, Some(JsNumber(1))),
      (pattern, Some(JsArray(Seq()))),
      (pattern, Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and None" in {
    process(pattern, None) shouldBe a[MatchError]
  }
}
