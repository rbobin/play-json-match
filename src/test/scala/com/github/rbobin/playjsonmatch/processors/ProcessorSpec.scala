package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.{PatternProcessor, UnitSpec}
import play.api.libs.json.JsValue

trait ProcessorSpec extends UnitSpec{

  val processor: PatternProcessor

  lazy val regexString = processor.regex.regex

  def process(s: String, o: Option[JsValue]) = processor.process(s, o)
}
