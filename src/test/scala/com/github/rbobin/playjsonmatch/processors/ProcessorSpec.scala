package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch._
import play.api.libs.json.JsValue

trait ProcessorSpec extends UnitSpec {

  val processor: PatternProcessor

  def process(s: String, o: Option[JsValue]) = processor.process(s, o)

  def assertAllMatchSkip(pairs: (String, Option[JsValue])*) =
    pairs.foreach { pair => process(pair._1, pair._2) shouldBe MatchSkip }

  def assertAllMatchSuccess(pairs: (String, Option[JsValue])*) =
    pairs.foreach { pair => process(pair._1, pair._2) shouldBe a[MatchSuccess] }

  def assertAllMatchError(pairs: (String, Option[JsValue])*) =
    pairs.foreach { pair => process(pair._1, pair._2) shouldBe a[MatchError] }

  def assertExceptionsThrown[T: Manifest](pairs: (String, Option[JsValue])*) =
    pairs.foreach { pair => a[T] should be thrownBy process(pair._1, pair._2) }
}
