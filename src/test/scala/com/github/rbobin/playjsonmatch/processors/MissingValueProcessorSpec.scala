package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

class MissingValueProcessorSpec extends ProcessorSpec {

  override val processor = MissingValueProcessor
  val pattern = "?"

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
    process(pattern, None) shouldBe a[MatchSuccess]
  }

  it should "fail with relevant pattern and any value" in {
    assertAllMatchError(
      (pattern, Some(JsNull)),
      (pattern, Some(JsString("1"))),
      (pattern, Some(JsNumber(0))),
      (pattern, Some(JsArray(Seq()))),
      (pattern, Some(JsObject(Seq())))
    )
  }
}
