package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import com.github.rbobin.playjsonmatch.processors.BooleanProcessor._
import play.api.libs.json._

class BooleanProcessorSpec extends UnitSpec {

  val regexString = BooleanProcessor.regex.regex

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    process(null, maybeJsValue) shouldBe MatchSkip
    process("", maybeJsValue)   shouldBe MatchSkip
    process("*", maybeJsValue)  shouldBe MatchSkip
    process("?!", maybeJsValue) shouldBe MatchSkip
  }

  it should "succeed with relevant pattern and JsBoolean" in {
    process(regexString, Some(JsBoolean(true)))  shouldBe a [MatchSuccess]
    process(regexString, Some(JsBoolean(false))) shouldBe a [MatchSuccess]
  }

  it should "fail with relevant pattern and anything else" in {
    process(regexString, None)                  shouldBe a [MatchError]
    process(regexString, Some(JsNumber(1)))     shouldBe a [MatchError]
    process(regexString, Some(JsString("..."))) shouldBe a [MatchError]
    process(regexString, Some(JsArray(Seq())))  shouldBe a [MatchError]
    process(regexString, Some(JsObject(Seq()))) shouldBe a [MatchError]
    process(regexString, Some(JsNull))          shouldBe a [MatchError]
  }
}
