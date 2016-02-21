package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsonPatternException
import play.api.libs.json._

class DateTimeProcessorSpec extends ProcessorSpec {

  override val processor = DateTimeProcessor

  "match" should "be skipped with irrelevant pattern" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      (" datetime(dd)", maybeJsValue),
      ("datetime MM", maybeJsValue),
      ("datetime(dd) ", maybeJsValue),
      ("datetime[dd]", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and matching datetime" in {
    assertAllMatchSuccess(
      ("datetime(yyyyMMdd)", Some(JsString("20150219"))),
      ("datetime(dd.MM.yy)", Some(JsString("19.02.15"))),
      ("datetime(dd:MM:yy)", Some(JsString("19:02:15"))),
      ("datetime(yyyy-MM-dd HH:mm)", Some(JsString("1986-04-08 12:30"))),
      ("datetime(yyyy-MM-dd HH:mm:ssXXX)", Some(JsString("2007-12-03 10:15:30+01:00"))),
      ("datetime(yyyy-MM-dd'T'HH:mm:ssXXX)", Some(JsString("2011-12-03T10:15:30Z"))),
      ("datetime(dd MM uuuu)", Some(JsString("30 12 2016"))),
      ("datetime(yyyy-MM-dd'T'HH:mm:ss)", Some(JsString("2011-12-03T10:15:30")))
    )
  }

  it should "fail with relevant pattern and not JsString" in {
    val pattern = "datetime(yyyyMMdd)"

    assertAllMatchError(
      (pattern, None),
      (pattern, Some(JsNull)),
      (pattern, Some(JsBoolean(false))),
      (pattern, Some(JsNumber(0))),
      (pattern, Some(JsArray(Seq()))),
      (pattern, Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and not matching datetime" in {
    assertAllMatchError(
      ("datetime(yyyyMMdd)", Some(JsString("20150250"))),
      ("datetime(dd.MM.yy)", Some(JsString("190215"))),
      ("datetime(dd:MM:yy)", Some(JsString("19.02.15"))),
      ("datetime(yyyy-MM-dd HH:mm)", Some(JsString("1986-04-08T12:30"))),
      ("datetime(yyyy-MM-dd HH:mm:ssXXX)", Some(JsString("2007-12-03 10:15:30"))),
      ("datetime(yyyy-MM-dd'T'HH:mm:ssXXX)", Some(JsString("2011-13-03T10:15:30Z"))),
      ("datetime(dd MM uuuu)", Some(JsString("30 1 2016"))),
      ("datetime(yyyy-MM-dd'T'HH:mm:ss)", Some(JsString("2011-12-03T10:15:30+03:00")))
    )
  }

  it should "throw an exception if format is not valid" in {
    assertExceptionsThrown[MalformedJsonPatternException](
      ("datetime(yyyy-MM-ddTHH:mm:ssXXX)", Some(JsString(""))),
      ("datetime(abcdefg)", Some(JsString("")))
    )
  }
}
