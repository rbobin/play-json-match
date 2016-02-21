package com.github.rbobin.playjsonmatch.processors

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDate, ZonedDateTime}

import com.github.rbobin.playjsonmatch.FailureMessages
import com.github.rbobin.playjsonmatch.utils.MalformedJsonPatternException
import play.api.libs.json.{JsString, JsValue}

object DateTimeProcessor extends SingleCapturingGroupProcessor {

  override val regex = "^datetime\\((.+)\\)$".r

  override def doMatch(formatString: String, maybeJsValue: Option[JsValue]) =
    maybeJsValue match {
      case Some(jsString: JsString) =>
        val formatter = getFormatter(formatString)
        val dateString = jsString.value

        if (verifyLocalDateFormat(formatter, dateString) || verifyZonedDateTimeFormat(formatter, dateString))
          success
        else
          fail(FailureMessages("datetimeFormatError", dateString, formatString))

      case x => fail(FailureMessages("wasNotString", x))
    }

  private def getFormatter(formatString: String) =
    try {
      DateTimeFormatter.ofPattern(formatString)
    } catch {
      case e: IllegalArgumentException =>
        throw new MalformedJsonPatternException(FailureMessages("datetimeFormatSyntaxError", formatString))
    }

  private def verifyLocalDateFormat(formatter: DateTimeFormatter, dateString: String): Boolean =
    try {
      LocalDate.parse(dateString, formatter)
      true
    } catch {
      case e: DateTimeParseException => false
    }

  private def verifyZonedDateTimeFormat(formatter: DateTimeFormatter, dateString: String): Boolean =
    try {
      ZonedDateTime.parse(dateString, formatter)
      true
    } catch {
      case e: DateTimeParseException => false
    }
}
