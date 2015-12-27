package com.github.rbobin.playjsonmatch

import org.scalatest.{PrivateMethodTester, Matchers, FlatSpec}
import play.api.libs.json._

class ErrorsSpec extends FlatSpec with Matchers with PrivateMethodTester {

  val testPath = Seq("root", "leaf")

  "prettifyPath" should "return \"/\" when invoked with empty seq" in {
    val path = Seq()
    val prettifyPath = PrivateMethod[String]('prettifyPath)
    val result = Errors invokePrivate prettifyPath(path)

    result should be ("/")
  }

  it should "return prettified path when invoked with non-empty seq" in {
    val path = Seq("[0]", "property", "test", "[2]")
    val prettifyPath = PrivateMethod[String]('prettifyPath)
    val result = Errors invokePrivate prettifyPath(path)

    result should be ("/ [0] / property / test / [2]")
  }

  "getJsClassName" should "return expected values" in {
    val getJsClassName = PrivateMethod[String]('getJsClassName)

    Errors invokePrivate getJsClassName(JsArray(Seq(JsNumber(1)))) should be ("Array")
    Errors invokePrivate getJsClassName(JsObject(Seq(("", JsNumber(1))))) should be ("Object")
    Errors invokePrivate getJsClassName(JsString("")) should be ("String")
    Errors invokePrivate getJsClassName(JsNumber.apply(1)) should be ("Number")
    Errors invokePrivate getJsClassName(JsBoolean(false)) should be ("Boolean")
    Errors invokePrivate getJsClassName(JsNull) should be ("Null")

  }

  "objectSupersetError" should "return empty list when invoked with empty seq" in {
    val keys = Seq()
    val result = Errors.objectSupersetError(keys, testPath)

    result should be (Nil)
  }

  ignore should "return sequence of one error message when invoked with non-empty seq" in {
    val keys = Seq("key1", "key2", "key3")
    val result = Errors.objectSupersetError(keys, testPath)

    result should be (Seq())
  }

}
