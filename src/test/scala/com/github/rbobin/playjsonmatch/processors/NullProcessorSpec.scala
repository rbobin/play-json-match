package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

class NullProcessorSpec extends ProcessorSpec {

  override val processor = NullProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      ("**", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and Null" in {
    process(regexString, Some(JsNull)) shouldBe a[MatchSuccess]
  }

  it should "fail with relevant pattern and anything but Null" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsString(""))),
      (regexString, Some(JsNumber(1))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}
