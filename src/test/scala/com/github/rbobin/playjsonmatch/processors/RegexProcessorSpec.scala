package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.MalformedJsonPatternException
import play.api.libs.json._

class RegexProcessorSpec extends ProcessorSpec {

  override val processor = RegexProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val jsString = Some(JsString("string"))

    assertAllMatchSkip(
      (null, jsString),
      ("", jsString),
      ("x", jsString),
      ("\\a\\", jsString),
      ("/", jsString),
      ("//", jsString),
      (" /pattern/", jsString),
      ("/pattern/ ", jsString),
      ("/\n/", jsString)
    )
  }

  it should "succeed with relevant pattern and matching string" in {
    assertAllMatchSuccess(
      ("/pattern/", Some(JsString("pattern"))),
      ("/^pattern$/", Some(JsString("pattern"))),
      ("/(.+)/", Some(JsString("abcde *[]"))),
      ("/(\\d?)(\\w+)/", Some(JsString("0 12_a"))),
      ("/ab.d/", Some(JsString("abcd"))),
      ("/\\d+$/", Some(JsString("1234567890"))),
      ("/\\W*/", Some(JsString(""))),
      ("/\\W*/", Some(JsString("@#$%"))),
      ("///", Some(JsString("/"))),
      ("/\\\\\\//", Some(JsString("a\\/b")))
    )
  }

  it should "fail with relevant pattern and not JsString" in {
    assertAllMatchError(
      ("/./", None),
      ("/./", Some(JsNull)),
      ("/./", Some(JsBoolean(false))),
      ("/./", Some(JsNumber(0))),
      ("/./", Some(JsArray(Seq()))),
      ("/./", Some(JsObject(Seq())))
    )
  }

  it should "fail with relevant pattern and not matching string" in {
    assertAllMatchError(
      ("/pattern/", Some(JsString("123abc"))),
      ("/^pattern$/", Some(JsString(" pattern "))),
      ("/(.+)/", Some(JsString(""))),
      ("/(\\d?)(\\w+)/", Some(JsString("^^"))),
      ("/ab.d/", Some(JsString("abc"))),
      ("/\\d+$/", Some(JsString("1234567890a"))),
      ("/\\\\\\//", Some(JsString("a\\b")))
    )
  }

  it should "throw an exception if the regex is not valid" in {
    a[MalformedJsonPatternException] should be thrownBy process("/\\x/", Some(JsString("")))
    a[MalformedJsonPatternException] should be thrownBy process("/)()(/", Some(JsString("")))
  }
}
