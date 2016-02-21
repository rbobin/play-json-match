package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.MatchError
import play.api.libs.json._

class AnyValueProcessorSpec extends ProcessorSpec {

  override val processor = AnyValueProcessor

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
      (regexString, Some(JsNull)),
      (regexString, Some(JsString(""))),
      (regexString, Some(JsNumber(1))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and None" in {
    process(regexString, None) shouldBe a[MatchError]
  }
}
