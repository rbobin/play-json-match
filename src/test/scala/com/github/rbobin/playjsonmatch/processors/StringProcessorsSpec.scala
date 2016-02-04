package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import play.api.libs.json._

class SimpleStringProcessorSpec extends ProcessorSpec {

  override val processor = SimpleStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("??", maybeJsValue),
      ("string:", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsString" in {
    assertAllMatchSuccess(
      (regexString, Some(JsString(""))),
      (regexString, Some(JsString("."))),
      (regexString, Some(JsString("string")))
    )
  }

  it should "fail with relevant pattern and anything but JsString" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsBoolean(false))),
      (regexString, Some(JsNumber(0))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}

class SizedStringProcessorSpec extends ProcessorSpec {

  override val processor = SizedStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("string", maybeJsValue),
      ("string:", maybeJsValue),
      ("string:1a", maybeJsValue),
      ("string:1.5", maybeJsValue),
      ("string:-1", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsString of correct size" in {
    assertAllMatchSuccess(
      ("string:0", Some(JsString(""))),
      ("string:1", Some(JsString("1"))),
      ("string:3", Some(JsString("abc"))),
      ("string:15", Some(JsString("123456789012345")))
    )
  }

  it should "fail with relevant pattern and not JsString" in {
    assertAllMatchError(
      ("string:0", None),
      ("string:1", Some(JsNull)),
      ("string:1000000", Some(JsBoolean(false))),
      ("string:9999", Some(JsNumber(0))),
      ("string:2", Some(JsArray(Seq()))),
      ("string:10", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and JsString of wrong size" in {
    assertAllMatchError(
      ("string:0", Some(JsString("."))),
      ("string:1", Some(JsString(""))),
      ("string:5", Some(JsString("abcdef")))
    )
  }
}

class BoundedStringProcessorSpec extends ProcessorSpec {

  override val processor = BoundedStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("string", maybeJsValue),
      ("string::", maybeJsValue),
      ("string:1a", maybeJsValue),
      ("string:1.5:3", maybeJsValue),
      ("string:-1:-5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsString of correct size" in {
    assertAllMatchSuccess(
      ("string:0:0", Some(JsString(""))),
      ("string:1:1", Some(JsString("."))),
      ("string:0:1", Some(JsString("a"))),
      ("string:0:20", Some(JsString("abcd"))),
      ("string:10:11", Some(JsString("1234567890"))),
      ("string:10:11", Some(JsString("12345678901"))),
      ("string:0:1000000", Some(JsString("a")))
    )
  }

  it should "fail with relevant pattern and not JsString" in {
    assertAllMatchError(
      ("string:0:0", None),
      ("string:5:1", Some(JsNull)),
      ("string:0:1000000", Some(JsBoolean(false))),
      ("string:777:9999", Some(JsNumber(0))),
      ("string:2:5", Some(JsArray(Seq()))),
      ("string:10:15", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and JsString of wrong size" in {
    assertAllMatchError(
      ("string:0:1", Some(JsString("??"))),
      ("string:1:5", Some(JsString(""))),
      ("string:5:10", Some(JsString("abcd"))),
      ("string:1:1", Some(JsString("ab"))),
      ("string:10:10", Some(JsString(",")))
    )
  }

  it should "throw an exception if min length is greater than max length" in {
    a[MalformedJsPatternException] should be thrownBy process("string:1:0", Some(JsString("1")))
    a[MalformedJsPatternException] should be thrownBy process("string:1000:500", Some(JsString("")))
  }
}

class LowerBoundedStringProcessorSpec extends ProcessorSpec {

  override val processor = LowerBoundedStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("string", maybeJsValue),
      ("string::", maybeJsValue),
      ("string:-3:", maybeJsValue),
      ("string:1.5:", maybeJsValue),
      ("string:5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsString of correct size" in {
    assertAllMatchSuccess(
      ("string:0:", Some(JsString(""))),
      ("string:1:", Some(JsString("."))),
      ("string:0:", Some(JsString("abcd"))),
      ("string:10:", Some(JsString("1234567890"))),
      ("string:10:", Some(JsString("12345678901"))),
      ("string:5:", Some(JsString("1000000")))
    )
  }

  it should "fail with relevant pattern and not JsString" in {
    assertAllMatchError(
      ("string:0:", None),
      ("string:5:", Some(JsNull)),
      ("string:0:", Some(JsBoolean(false))),
      ("string:777:", Some(JsNumber(0))),
      ("string:2:", Some(JsArray(Seq()))),
      ("string:10:", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and JsString of wrong size" in {
    assertAllMatchError(
      ("string:3:", Some(JsString("??"))),
      ("string:1:", Some(JsString(""))),
      ("string:5:", Some(JsString("abcd"))),
      ("string:1:", Some(JsString(""))),
      ("string:10:", Some(JsString(",")))
    )
  }
}

class UpperBoundedStringProcessorSpec extends ProcessorSpec {

  override val processor = UpperBoundedStringProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("string", maybeJsValue),
      ("string::", maybeJsValue),
      ("string::-20", maybeJsValue),
      ("string::1.0", maybeJsValue),
      ("string:1:5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsString of correct size" in {
    assertAllMatchSuccess(
      ("string::0", Some(JsString(""))),
      ("string::1", Some(JsString("."))),
      ("string::1000", Some(JsString("abcd"))),
      ("string::10", Some(JsString("1234567890"))),
      ("string::10", Some(JsString("123456789"))),
      ("string::15", Some(JsString("1000000")))
    )
  }

  it should "fail with relevant pattern and not JsString" in {
    assertAllMatchError(
      ("string::0", None),
      ("string::5", Some(JsNull)),
      ("string::0", Some(JsBoolean(false))),
      ("string::777", Some(JsNumber(0))),
      ("string::2", Some(JsArray(Seq()))),
      ("string::10", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and JsString of wrong size" in {
    assertAllMatchError(
      ("string::3", Some(JsString("????"))),
      ("string::0", Some(JsString("$"))),
      ("string::5", Some(JsString("abcdef"))),
      ("string::1", Some(JsString("qwertyuiop"))),
      ("string::10", Some(JsString("12345678901")))
    )
  }
}
