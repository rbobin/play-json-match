package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

class MissingValueProcessorSpec extends ProcessorSpec {

  override val processor = MissingValueProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("??", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and None" in {
    process(regexString, None) shouldBe a[MatchSuccess]
  }

  it should "fail with relevant pattern and any value" in {
    assertAllMatchError(
      (regexString, Some(JsNull)),
      (regexString, Some(JsString("1"))),
      (regexString, Some(JsNumber(0))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}
