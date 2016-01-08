package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.utils.Utils
import org.scalatest.{Matchers, FlatSpec}
import play.api.libs.json._

class UtilsSpec extends FlatSpec with Matchers {

  "getJsClassName" should "return expected values" in {
    Utils.getJsClassName(JsArray(Seq(JsNumber(1))))        should be ("Array")
    Utils.getJsClassName(JsObject(Seq(("", JsNumber(1))))) should be ("Object")
    Utils.getJsClassName(JsString(""))                     should be ("String")
    Utils.getJsClassName(JsNumber.apply(1))                should be ("Number")
    Utils.getJsClassName(JsBoolean(false))                 should be ("Boolean")
    Utils.getJsClassName(JsNull)                           should be ("Null")
  }

  "getStringRepresentation" should "return expected values" in {

  }
}
