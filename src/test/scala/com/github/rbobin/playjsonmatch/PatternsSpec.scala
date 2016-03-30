package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json.{JsNull, JsString, JsBoolean}

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

  val path = Seq()

  ignore should "return empty list if match found" in {
    Matcher.processPatterns("boolean", Some(JsBoolean(false)), path) shouldBe NO_ERRORS
    Matcher.processPatterns("boolean|null|number", Some(JsNull), path) shouldBe NO_ERRORS
    Matcher.processPatterns("string:1:5", Some(JsString("123")), path) shouldBe NO_ERRORS
    Matcher.processPatterns("missing|array::20", None, path) shouldBe NO_ERRORS
  }
}
