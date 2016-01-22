package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.processors.AnyValueProcessor._
import com.github.rbobin.playjsonmatch.{MatchError, MatchSkip, MatchSuccess, UnitSpec}
import play.api.libs.json._

class AnyValueProcessorSpec extends UnitSpec {

  val regexString = AnyValueProcessor.regex.regex

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue) shouldBe MatchSkip
    process("", maybeJsValue)   shouldBe MatchSkip
    process(".", maybeJsValue)  shouldBe MatchSkip
    process("**", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and any value" in {
    process(regexString, Some(JsNull))          shouldBe a [MatchSuccess]
    process(regexString, Some(JsString("")))    shouldBe a [MatchSuccess]
    process(regexString, Some(JsNumber(1)))     shouldBe a [MatchSuccess]
    process(regexString, Some(JsArray(Seq())))  shouldBe a [MatchSuccess]
    process(regexString, Some(JsObject(Seq()))) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and None" in {
    process(regexString, None) shouldBe a [MatchError]
  }
}
