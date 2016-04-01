package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.Matcher.ErrorsOrSuccess
import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json.{JsBoolean, JsNull, JsNumber, JsString}

class PatternsSpec extends UnitSpec {

  "splitPatterns" should "return list of strings when split is not empty" in {
    val splitPatterns = PrivateMethod[List[String]]('splitPatterns)

    val result1 = Matcher invokePrivate splitPatterns("one|two|three")
    result1.length shouldBe 3

    val result2 = Matcher invokePrivate splitPatterns("|one||two|")
    result2.length shouldBe 2

    val result3 = Matcher invokePrivate splitPatterns("|one|")
    result3.length shouldBe 1
    result3.head shouldBe "one"

    val result4 = Matcher invokePrivate splitPatterns("one")
    result4.length shouldBe 1
    result3.head shouldBe "one"
  }

  it should "return throw an exception when split is empty" in {
    val splitPatterns = PrivateMethod[List[String]]('splitPatterns)

    a[JsMatchException] should be thrownBy Matcher.invokePrivate(splitPatterns(""))
    a[JsMatchException] should be thrownBy Matcher.invokePrivate(splitPatterns("|"))
    a[JsMatchException] should be thrownBy Matcher.invokePrivate(splitPatterns("|||"))
  }

  "getMatchResults" should "return Right if at least one match succeeded" in {
    val getMatchResults = PrivateMethod[ErrorsOrSuccess]('getMatchResults)

    Matcher invokePrivate getMatchResults("boolean", Some(JsBoolean(false))) shouldBe a [Right[_, _]]
    Matcher invokePrivate getMatchResults("boolean|null|number", Some(JsNull)) shouldBe a [Right[_, _]]
    Matcher invokePrivate getMatchResults("string:1:5", Some(JsString("123"))) shouldBe a [Right[_, _]]
    Matcher invokePrivate getMatchResults("?|array::20", None) shouldBe a [Right[_, _]]
  }

  it should "return Left if all matches failed" in {
    val getMatchResults = PrivateMethod[ErrorsOrSuccess]('getMatchResults)

    Matcher invokePrivate getMatchResults("string:0:5", Some(JsBoolean(true))) shouldBe a [Left[_, _]]
    Matcher invokePrivate getMatchResults("object:5:|array", Some(JsNumber(500))) shouldBe a [Left[_, _]]
    Matcher invokePrivate getMatchResults("boolean|boolean|number", None) shouldBe a [Left[_, _]]
    Matcher invokePrivate getMatchResults("?", Some(JsBoolean(true))) shouldBe a [Left[_, _]]
  }
}
