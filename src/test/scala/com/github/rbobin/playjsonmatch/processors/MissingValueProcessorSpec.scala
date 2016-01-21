package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{MatchSuccess, MatchSkip, UnitSpec, MatchError}
import com.github.rbobin.playjsonmatch.processors.MissingValueProcessor.process
import play.api.libs.json._

class MissingValueProcessorSpec extends UnitSpec {

  val pattern = MissingValueProcessor.patternString

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue) shouldBe MatchSkip
    process("", maybeJsValue)   shouldBe MatchSkip
    process("a", maybeJsValue)  shouldBe MatchSkip
    process("??", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and None" in {
    process(pattern, None) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and any value" in {
    process(pattern, Some(JsNull))          shouldBe a [MatchError]
    process(pattern, Some(JsString("1")))   shouldBe a [MatchError]
    process(pattern, Some(JsNumber(0)))     shouldBe a [MatchError]
    process(pattern, Some(JsArray(Seq())))  shouldBe a [MatchError]
    process(pattern, Some(JsObject(Seq()))) shouldBe a [MatchError]
  }

}
