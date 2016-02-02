package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

class NullProcessorSpec extends ProcessorSpec {

  override val processor = NullProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue) shouldBe MatchSkip
    process("", maybeJsValue)   shouldBe MatchSkip
    process(".", maybeJsValue)  shouldBe MatchSkip
    process("**", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and Null" in {
    process(regexString, Some(JsNull)) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and anything but Null" in {
    process(regexString, None)                  shouldBe a [MatchError]
    process(regexString, Some(JsString("")))    shouldBe a [MatchError]
    process(regexString, Some(JsNumber(1)))     shouldBe a [MatchError]
    process(regexString, Some(JsArray(Seq())))  shouldBe a [MatchError]
    process(regexString, Some(JsObject(Seq()))) shouldBe a [MatchError]
  }
}
