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

class SizedStringProcessorSpec extends ProcessorSpec {

  val processor = SizedStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue)         shouldBe MatchSkip
    process("", maybeJsValue)           shouldBe MatchSkip
    process("a", maybeJsValue)          shouldBe MatchSkip
    process("string", maybeJsValue)     shouldBe MatchSkip
    process("string:", maybeJsValue)    shouldBe MatchSkip
    process("string:1a", maybeJsValue)  shouldBe MatchSkip
    process("string:1.5", maybeJsValue) shouldBe MatchSkip
    process("string:-1", maybeJsValue)  shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and JsString of correct size" in {
    process("string:0", Some(JsString("")))                 shouldBe a [MatchSuccess]
    process("string:1", Some(JsString("1")))                shouldBe a [MatchSuccess]
    process("string:3", Some(JsString("abc")))              shouldBe a [MatchSuccess]
    process("string:15", Some(JsString("123456789012345"))) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and not JsString" in {
    process("string:0", None)                         shouldBe a [MatchError]
    process("string:1", Some(JsNull))                 shouldBe a [MatchError]
    process("string:1000000", Some(JsBoolean(false))) shouldBe a [MatchError]
    process("string:9999", Some(JsNumber(0)))         shouldBe a [MatchError]
    process("string:2", Some(JsArray(Seq())))         shouldBe a [MatchError]
    process("string:10", Some(JsObject(Seq())))       shouldBe a [MatchError]
  }

  it should "fail with relevant pattern and JsString of wrong size" in {
    process("string:0", Some(JsString(".")))      shouldBe a [MatchError]
    process("string:1", Some(JsString("")))       shouldBe a [MatchError]
    process("string:5", Some(JsString("abcdef"))) shouldBe a [MatchError]
  }
}
