package com.github.rbobin.playjsonmatch.processors

import java.util.regex.PatternSyntaxException

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json.{JsString, JsValue}

import scala.util.matching.Regex

object RegexProcessor extends SingleCapturingGroupProcessor {
  override val regex = "^\\/(.+)\\/$".r

  override def doMatch(matched: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) =>
        val customRegex = getRegex(matched)
        val testedString = jsString.value

        if (customRegex.findFirstIn(testedString).isDefined) success
        else fail(FailureMessages("didNotMatch", testedString, matched))

      case x => fail(FailureMessages("wasNotString"))
    }

  private def getRegex(regexString: String): Regex =
    try {
      regexString.r
    } catch {
      case e: PatternSyntaxException =>
        throw new JsMatchException(FailureMessages("regexSyntaxError", e.getMessage))
    }
}
