package com.github.rbobin.playjsonmatch.processors

import play.api.libs.json._

import scala.util.Random

class SimpleObjectProcessorSpec extends ProcessorSpec {

  override val processor = SimpleObjectProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      (" object ", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsObject" in {
    assertAllMatchSuccess(
      (regexString, Some(JsObject(Seq()))),
      (regexString, Some(JsObject(Seq(("field", JsNull))))),
      (regexString, Some(JsObject(Seq(("1", JsNumber(1)), ("2", JsBoolean(false))))))
    )
  }

  it should "fail with relevant pattern and anything but JsObject" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsNumber(1000))),
      (regexString, Some(JsString("!"))),
      (regexString, Some(JsBoolean(true))),
      (regexString, Some(JsArray(Seq())))
    )
  }
}

class SizedObjectProcessorSpec extends ProcessorSpec {

  override val processor = SizedObjectProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("object", maybeJsValue),
      ("object:", maybeJsValue),
      ("object:x", maybeJsValue),
      ("object:1.5", maybeJsValue),
      ("object:-1", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsObject of correct size" in {
    assertAllMatchSuccess(
      ("object:0", Some(JsObject(Seq()))),
      ("object:1", Some(JsObject(Seq(("a", JsString("")))))),
      ("object:2", Some(JsObject(Seq(("1", JsString("1")), ("2", JsObject(Seq())))))),
      ("object:15", Some(JsObject(Seq.fill(15)((Random.nextInt().toString, JsNull)))))
    )
  }

  it should "fail with relevant pattern and not JsObject" in {
    assertAllMatchError(
      ("object:0", None),
      ("object:1", Some(JsNull)),
      ("object:1000000", Some(JsBoolean(false))),
      ("object:9999", Some(JsNumber(0))),
      ("object:2", Some(JsArray(Seq()))),
      ("object:10", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsObject of wrong size" in {
    assertAllMatchError(
      ("object:1", Some(JsObject(Seq()))),
      ("object:2", Some(JsObject(Seq((".", JsNumber(9)))))),
      ("object:12", Some(JsObject(Seq.fill(11)((Random.nextInt().toString, JsNull)))))
    )
  }
}
