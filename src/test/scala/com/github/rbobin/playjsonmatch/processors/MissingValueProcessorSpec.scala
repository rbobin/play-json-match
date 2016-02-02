package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json._

class MissingValueProcessorSpec extends ProcessorSpec {

  override val processor = MissingValueProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue) shouldBe MatchSkip
    process("", maybeJsValue)   shouldBe MatchSkip
    process("a", maybeJsValue)  shouldBe MatchSkip
    process("??", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and None" in {
    process(regexString, None) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and any value" in {
    process(regexString, Some(JsNull))          shouldBe a [MatchError]
    process(regexString, Some(JsString("1")))   shouldBe a [MatchError]
    process(regexString, Some(JsNumber(0)))     shouldBe a [MatchError]
    process(regexString, Some(JsArray(Seq())))  shouldBe a [MatchError]
    process(regexString, Some(JsObject(Seq()))) shouldBe a [MatchError]
  }

}
