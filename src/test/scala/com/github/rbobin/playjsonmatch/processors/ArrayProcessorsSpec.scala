package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.JsMatchException
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

class BoundedArrayProcessorSpec extends ProcessorSpec {

  override val processor = BoundedArrayProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("array", maybeJsValue),
      ("array::", maybeJsValue),
      ("array:x:2", maybeJsValue),
      ("array:1.5:1.1", maybeJsValue),
      ("array:-1:3", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsArray of correct size" in {
    assertAllMatchSuccess(
      ("array:0:0", Some(JsArray(Seq()))),
      ("array:5:10", Some(JsArray(Seq.fill(5)(JsNull)))),
      ("array:5:10", Some(JsArray(Seq.fill(7)(JsNull)))),
      ("array:5:10", Some(JsArray(Seq.fill(10)(JsNull))))
    )
  }

  it should "fail with relevant pattern and not JsArray" in {
    assertAllMatchError(
      ("array:0:0", None),
      ("array:1:2", Some(JsNull)),
      ("array:1000000:0", Some(JsBoolean(false))),
      ("array:9999:10000", Some(JsNumber(0))),
      ("array:2:5", Some(JsObject(Seq()))),
      ("array:10:12", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsArray of wrong size" in {
    assertAllMatchError(
      ("array:0:0", Some(JsArray(Seq(JsNumber(9))))),
      ("array:1:10", Some(JsArray(Seq()))),
      ("array:5:10", Some(JsArray(Seq.fill(4)(JsNull)))),
      ("array:5:10", Some(JsArray(Seq.fill(11)(JsNull))))
    )
  }

  it should "throw an exception if min length is greater than max length" in {
    assertExceptionsThrown[JsMatchException](
      ("array:1:0", Some(JsArray(Seq()))),
      ("array:1000:500", Some(JsArray(Seq())))
    )
  }
}

class LowerBoundedArrayProcessorSpec extends ProcessorSpec {

  override val processor = LowerBoundedArrayProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("array", maybeJsValue),
      ("array::", maybeJsValue),
      ("array:-3:", maybeJsValue),
      ("array:1.5:", maybeJsValue),
      ("array:5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsArray of correct size" in {
    assertAllMatchSuccess(
      ("array:0:", Some(JsArray(Seq()))),
      ("array:1:", Some(JsArray(Seq(JsNumber(9))))),
      ("array:0:", Some(JsArray(Seq(JsNumber(9))))),
      ("array:5:", Some(JsArray(Seq.fill(5)(JsNull)))),
      ("array:5:", Some(JsArray(Seq.fill(9)(JsNull))))
    )
  }

  it should "fail with relevant pattern and not JsArray" in {
    assertAllMatchError(
      ("array:0:", None),
      ("array:5:", Some(JsNull)),
      ("array:0:", Some(JsBoolean(false))),
      ("array:100:", Some(JsNumber(0))),
      ("array:2:", Some(JsObject(Seq()))),
      ("array:10:", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsArray of wrong size" in {
    assertAllMatchError(
      ("array:1:", Some(JsArray(Seq()))),
      (s"array:${Int.MaxValue}:", Some(JsArray(Seq()))),
      ("array:6:", Some(JsArray(Seq.fill(5)(JsNull)))),
      ("array:15:", Some(JsArray(Seq.fill(5)(JsNull)))),
      ("array:60:", Some(JsArray(Seq.fill(5)(JsNull))))
    )
  }
}

class UpperBoundedArrayProcessorSpec extends ProcessorSpec {

  override val processor = UpperBoundedArrayProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("array", maybeJsValue),
      ("array::", maybeJsValue),
      ("array::-3", maybeJsValue),
      ("array::1.5", maybeJsValue),
      ("array:5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsArray of correct size" in {
    assertAllMatchSuccess(
      ("array::0", Some(JsArray(Seq()))),
      ("array::1", Some(JsArray(Seq(JsNumber(9))))),
      ("array::0", Some(JsArray(Seq()))),
      ("array::5", Some(JsArray(Seq.fill(5)(JsNull)))),
      ("array::5", Some(JsArray(Seq.fill(3)(JsNull))))
    )
  }

  it should "fail with relevant pattern and not JsArray" in {
    assertAllMatchError(
      ("array::0", None),
      ("array::5", Some(JsNull)),
      ("array::0", Some(JsBoolean(false))),
      ("array::100", Some(JsNumber(0))),
      (s"array::${Int.MaxValue}", Some(JsObject(Seq()))),
      ("array::10", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsArray of wrong size" in {
    assertAllMatchError(
      ("array::0", Some(JsArray(Seq(JsNumber(9))))),
      ("array::6", Some(JsArray(Seq.fill(7)(JsNull)))),
      ("array::15", Some(JsArray(Seq.fill(20)(JsNull))))
    )
  }
}