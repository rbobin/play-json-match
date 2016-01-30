package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{MatchError, MatchSkip, MatchSuccess}
import play.api.libs.json._

class SimpleStringProcessorSpec extends ProcessorSpec {

  val processor = SimpleStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue)      shouldBe MatchSkip
    process("", maybeJsValue)        shouldBe MatchSkip
    process("??", maybeJsValue)      shouldBe MatchSkip
    process("string:", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and JsString" in {
    process(regexString, Some(JsString("")))       shouldBe a [MatchSuccess]
    process(regexString, Some(JsString(".")))      shouldBe a [MatchSuccess]
    process(regexString, Some(JsString("string"))) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and anything but JsString" in {
    process(regexString, None)                   shouldBe a [MatchError]
    process(regexString, Some(JsNull))           shouldBe a [MatchError]
    process(regexString, Some(JsBoolean(false))) shouldBe a [MatchError]
    process(regexString, Some(JsNumber(0)))      shouldBe a [MatchError]
    process(regexString, Some(JsArray(Seq())))   shouldBe a [MatchError]
    process(regexString, Some(JsObject(Seq())))  shouldBe a [MatchError]
  }

}

class SizedObjectProcessorSpec
