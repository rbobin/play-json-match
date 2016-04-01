package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

class NullProcessorSpec extends ProcessorSpec {

  override val processor = NullProcessor
  val pattern = "null"

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
    process(pattern, Some(JsNull)) shouldBe a[MatchSuccess]
  }

  it should "fail with relevant pattern and anything but Null" in {
    assertAllMatchError(
      (pattern, None),
      (pattern, Some(JsString(""))),
      (pattern, Some(JsNumber(1))),
      (pattern, Some(JsArray(Seq()))),
      (pattern, Some(JsObject(Seq())))
    )
  }
}
