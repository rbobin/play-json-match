package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.processors.AnyValueProcessor._
import com.github.rbobin.playjsonmatch.{MatchError, MatchSkip, MatchSuccess, UnitSpec}
import play.api.libs.json._

class AnyValueProcessorSpec extends UnitSpec {

  val pattern = AnyValueProcessor.patternString

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue) shouldBe MatchSkip
    process("", maybeJsValue)   shouldBe MatchSkip
    process(".", maybeJsValue)  shouldBe MatchSkip
    process("**", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and any value" in {
    process(pattern, Some(JsNull))          shouldBe a [MatchSuccess]
    process(pattern, Some(JsString("")))    shouldBe a [MatchSuccess]
    process(pattern, Some(JsNumber(1)))     shouldBe a [MatchSuccess]
    process(pattern, Some(JsArray(Seq())))  shouldBe a [MatchSuccess]
    process(pattern, Some(JsObject(Seq()))) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and None" in {
    process(pattern, None) shouldBe a [MatchError]
  }
}
