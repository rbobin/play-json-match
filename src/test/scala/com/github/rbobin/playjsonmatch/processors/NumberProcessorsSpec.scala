package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsPatternException
import play.api.libs.json._

class SimpleNumberProcessorSpec extends ProcessorSpec {

  override val processor = SimpleNumberProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (".", maybeJsValue),
      ("number:", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsNumber" in {
    assertAllMatchSuccess(
      (regexString, Some(JsNumber(0))),
      (regexString, Some(JsNumber(1))),
      (regexString, Some(JsNumber(-1))),
      (regexString, Some(JsNumber(BigDecimal(5, 5)))),
      (regexString, Some(JsNumber(Integer.MIN_VALUE)))
    )
  }

  it should "fail with relevant pattern and anything but JsNumber" in {
    assertAllMatchError(
      (regexString, None),
      (regexString, Some(JsNull)),
      (regexString, Some(JsBoolean(false))),
      (regexString, Some(JsString("!"))),
      (regexString, Some(JsArray(Seq()))),
      (regexString, Some(JsObject(Seq())))
    )
  }
}

class NumberInRangeProcessorSpec extends ProcessorSpec {

  override val processor = NumberInRangeProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("number", maybeJsValue),
      ("number:1:", maybeJsValue),
      ("number:1o", maybeJsValue),
      ("number:1.5.1:3", maybeJsValue),
      ("number:--1:-5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsNumber in correct range" in {
    assertAllMatchSuccess(
      ("number:0:0", Some(JsNumber(0))),
      ("number:1:1", Some(JsNumber(1))),
      ("number:0:1", Some(JsNumber(0))),
      ("number:0:1", Some(JsNumber(1))),
      ("number:0:20", Some(JsNumber(10))),
      ("number:10:12", Some(JsNumber(11))),
      ("number:10.0:10", Some(JsNumber(10))),
      ("number:10:10.0", Some(JsNumber(10))),
      ("number:-10:11", Some(JsNumber(-10))),
      ("number:-11:-10", Some(JsNumber(-10.01))),
      ("number:1.0:2.0", Some(JsNumber(2))),
      ("number:1.2:1.2", Some(JsNumber(1.2))),
      ("number:1.2:2.2", Some(JsNumber(1.2))),
      ("number:1.2:2.2", Some(JsNumber(2.2))),
      ("number:.2:.3", Some(JsNumber(0.25))),
      ("number:0.005:0.1", Some(JsNumber(0.05))),
      ("number:-0.2:-0.1", Some(JsNumber(-0.15))),
      ("number:-0:0", Some(JsNumber(0))),
      ("number:0:1000000", Some(JsNumber(1)))
    )
  }

  it should "fail with relevant pattern and not JsNumber" in {
    assertAllMatchError(
      ("number:0:0", None),
      ("number:5:1", Some(JsNull)),
      ("number:0:1000000", Some(JsBoolean(false))),
      ("number:-1000.1:-1.10000", Some(JsString("-2"))),
      ("number:2:5", Some(JsArray(Seq()))),
      ("number:10:15", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and JsNumber not in correct range" in {
    assertAllMatchError(
      ("number:0:1", Some(JsNumber(-1))),
      ("number:1:5", Some(JsNumber(0.999))),
      ("number:5.0:10.0", Some(JsNumber(10.001))),
      ("number:-1:1", Some(JsNumber(2))),
      ("number:10:10.0", Some(JsNumber(0)))
    )
  }

  it should "throw an exception if min is greater than max" in {
    a[MalformedJsPatternException] should be thrownBy process("number:1:0", Some(JsNumber(0)))
    a[MalformedJsPatternException] should be thrownBy process("number:-1000:-1000.0001", Some(JsNumber(0)))
  }
}

class LowerBoundedNumberProcessorSpec extends ProcessorSpec {

  override val processor = LowerBoundedNumberProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("number", maybeJsValue),
      ("number::1", maybeJsValue),
      ("number:1.:", maybeJsValue),
      ("number:1z:", maybeJsValue),
      ("number:1.5.1:3", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsNumber in correct range" in {
    assertAllMatchSuccess(
      ("number:0:", Some(JsNumber(0))),
      ("number:0.0000:", Some(JsNumber(0))),
      ("number:-0.0000:", Some(JsNumber(0))),
      ("number:1:", Some(JsNumber(1))),
      ("number:.0:", Some(JsNumber(0.1))),
      ("number:.1:", Some(JsNumber(1.1))),
      ("number:0.005:", Some(JsNumber(0.05))),
      ("number:-10.1:", Some(JsNumber(-10)))
    )
  }

  it should "fail with relevant pattern and not JsNumber" in {
    assertAllMatchError(
      ("number:0:", None),
      ("number:5:", Some(JsNull)),
      ("number:0:", Some(JsBoolean(false))),
      ("number:-1000.1:", Some(JsString("-2"))),
      ("number:2:", Some(JsArray(Seq()))),
      ("number:10:", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and JsNumber not in correct range" in {
    assertAllMatchError(
      ("number:0:", Some(JsNumber(-1))),
      ("number:1:", Some(JsNumber(0.999))),
      ("number:5.0:", Some(JsNumber(4.99))),
      ("number:-1:", Some(JsNumber(-2))),
      ("number:10:", Some(JsNumber(0)))
    )
  }
}
