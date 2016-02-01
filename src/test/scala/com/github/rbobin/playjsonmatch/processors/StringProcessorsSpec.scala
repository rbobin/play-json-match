package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
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

class BoundedStringProcessorSpec extends ProcessorSpec {

  val processor = BoundedStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue)           shouldBe MatchSkip
    process("", maybeJsValue)             shouldBe MatchSkip
    process("a", maybeJsValue)            shouldBe MatchSkip
    process("string", maybeJsValue)       shouldBe MatchSkip
    process("string::", maybeJsValue)     shouldBe MatchSkip
    process("string:1a", maybeJsValue)    shouldBe MatchSkip
    process("string:1.5:3", maybeJsValue) shouldBe MatchSkip
    process("string:-1:-5", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and JsString of correct size" in {
    process("string:0:0", Some(JsString("")))              shouldBe a [MatchSuccess]
    process("string:1:1", Some(JsString(".")))             shouldBe a [MatchSuccess]
    process("string:0:1", Some(JsString("a")))             shouldBe a [MatchSuccess]
    process("string:0:20", Some(JsString("abcd")))         shouldBe a [MatchSuccess]
    process("string:10:11", Some(JsString("1234567890")))  shouldBe a [MatchSuccess]
    process("string:10:11", Some(JsString("12345678901"))) shouldBe a [MatchSuccess]
    process("string:0:1000000", Some(JsString("a")))       shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and not JsString" in {
    process("string:0:0", None)                         shouldBe a [MatchError]
    process("string:5:1", Some(JsNull))                 shouldBe a [MatchError]
    process("string:0:1000000", Some(JsBoolean(false))) shouldBe a [MatchError]
    process("string:777:9999", Some(JsNumber(0)))       shouldBe a [MatchError]
    process("string:2:5", Some(JsArray(Seq())))         shouldBe a [MatchError]
    process("string:10:15", Some(JsObject(Seq())))      shouldBe a [MatchError]
  }

  it should "fail with relevant pattern and JsString of wrong size" in {
    process("string:0:1", Some(JsString("??")))      shouldBe a [MatchError]
    process("string:1:5", Some(JsString("")))        shouldBe a [MatchError]
    process("string:5:10", Some(JsString("abcd")))   shouldBe a [MatchError]
    process("string:1:1", Some(JsString("")))        shouldBe a [MatchError]
    process("string:10:10", Some(JsString(",")))     shouldBe a [MatchError]
  }

  it should "throw an exception if min length is greater than max length" in {
    a [MalformedJsPatternException] should be thrownBy process("string:1:0", Some(JsString("1")))
    a [MalformedJsPatternException] should be thrownBy process("string:1000:500", Some(JsString("")))
  }
}
