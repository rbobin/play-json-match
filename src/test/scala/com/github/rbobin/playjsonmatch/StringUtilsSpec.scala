package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.utils.StringUtils
import play.api.libs.json._

class StringUtilsSpec extends UnitSpec {

  "getJsClassName" should "return expected values" in {
    StringUtils.getJsClassName(JsArray(Seq(JsNumber(1))))        should be ("Array")
    StringUtils.getJsClassName(JsObject(Seq(("", JsNumber(1))))) should be ("Object")
    StringUtils.getJsClassName(JsString(""))                     should be ("String")
    StringUtils.getJsClassName(JsNumber.apply(1))                should be ("Number")
    StringUtils.getJsClassName(JsBoolean(false))                 should be ("Boolean")
    StringUtils.getJsClassName(JsNull)                           should be ("Null")
  }

  "getStringRepresentation" should "return expected values" in {

  }

  "prettifyPath" should "return \"/\" when invoked with empty seq" in {
    val path = Seq()
    val prettifyPath = PrivateMethod[String]('prettifyPath)
    val result = StringUtils invokePrivate prettifyPath(path)

    result should be ("[ / ]")
  }

  it should "return prettified path when invoked with non-empty seq" in {
    val path = Seq("[0]", "property", "test", "[2]")
    val prettifyPath = PrivateMethod[String]('prettifyPath)
    val result = StringUtils invokePrivate prettifyPath(path)

    result should be ("[ / [0] / property / test / [2] ]")
  }
}
