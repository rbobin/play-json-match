package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

class SimpleArrayProcessorSpec extends ProcessorSpec {

  override val processor = SimpleArrayProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      (" array ", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsArray" in {
    assertAllMatchSuccess(
      (regexString, Some(JsArray())),
      (regexString, Some(JsArray(Seq(JsBoolean(true))))),
      (regexString, Some(JsArray(Seq.fill(5)(JsNull))))
    )
  }

  it should "fail with relevant pattern and anything but JsArray" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsNumber(1000))),
      (regexString, Some(JsString("!"))),
      (regexString, Some(JsBoolean(true))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}

class SizedArrayProcessorSpec extends ProcessorSpec {

  override val processor = SizedArrayProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("array", maybeJsValue),
      ("array:", maybeJsValue),
      ("array:x", maybeJsValue),
      ("array:1.5", maybeJsValue),
      ("array:-1", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsArray of correct size" in {
    assertAllMatchSuccess(
      ("array:0", Some(JsArray())),
      ("array:1", Some(JsArray(Seq(JsBoolean(true))))),
      ("array:4", Some(JsArray(Seq.fill(4)(JsNull))))
    )
  }

  it should "fail with relevant pattern and not JsArray" in {
    assertAllMatchError(
      ("array:0", None),
      ("array:1", Some(JsNull)),
      ("array:10000", Some(JsBoolean(false))),
      ("array:9999", Some(JsNumber(0))),
      ("array:2", Some(JsObject(Seq()))),
      ("array:10", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsArray of wrong size" in {
    assertAllMatchError(
      ("array:1", Some(JsArray(Seq()))),
      ("array:2", Some(JsArray(Seq(JsBoolean(true))))),
      ("array:12", Some(JsArray(Seq.fill(4)(JsNull))))
    )
  }
}