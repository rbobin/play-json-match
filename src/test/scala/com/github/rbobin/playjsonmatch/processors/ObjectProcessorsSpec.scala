package com.github.rbobin.playjsonmatch.processors

import com.github.rbobin.playjsonmatch.utils.JsMatchException
import play.api.libs.json._

import scala.util.Random

class SimpleObjectProcessorSpec extends ProcessorSpec {

  override val processor = SimpleObjectProcessor
  val pattern = "object"

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
      (pattern, Some(JsObject(Seq()))),
      (pattern, Some(JsObject(Seq(("field", JsNull))))),
      (pattern, Some(JsObject(Seq(("1", JsNumber(1)), ("2", JsBoolean(false))))))
    )
  }

  it should "fail with relevant pattern and anything but JsObject" in {
    assertAllMatchError(
      (pattern, None),
      (pattern, Some(JsNull)),
      (pattern, Some(JsNumber(1000))),
      (pattern, Some(JsString("!"))),
      (pattern, Some(JsBoolean(true))),
      (pattern, Some(JsArray(Seq())))
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

class BoundedObjectProcessorSpec extends ProcessorSpec {

  override val processor = BoundedObjectProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("object", maybeJsValue),
      ("object::", maybeJsValue),
      ("object:x:2", maybeJsValue),
      ("object:1.5:1.1", maybeJsValue),
      ("object:-1:3", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsObject of correct size" in {
    assertAllMatchSuccess(
      ("object:0:0", Some(JsObject(Seq()))),
      ("object:5:10", Some(JsObject(Seq.fill(5)((Random.nextInt().toString, JsNull))))),
      ("object:5:10", Some(JsObject(Seq.fill(7)((Random.nextInt().toString, JsNull))))),
      ("object:5:10", Some(JsObject(Seq.fill(10)((Random.nextInt().toString, JsNull)))))
    )
  }

  it should "fail with relevant pattern and not JsObject" in {
    assertAllMatchError(
      ("object:0:0", None),
      ("object:1:2", Some(JsNull)),
      ("object:1000000:0", Some(JsBoolean(false))),
      ("object:9999:10000", Some(JsNumber(0))),
      ("object:2:5", Some(JsArray(Seq()))),
      ("object:10:12", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsObject of wrong size" in {
    assertAllMatchError(
      ("object:0:0", Some(JsObject(Seq((".", JsNumber(9)))))),
      ("object:1:10", Some(JsObject(Seq()))),
      ("object:5:10", Some(JsObject(Seq.fill(4)((Random.nextInt().toString, JsNull))))),
      ("object:5:10", Some(JsObject(Seq.fill(11)((Random.nextInt().toString, JsNull)))))
    )
  }

  it should "throw an exception if min length is greater than max length" in {
    assertExceptionsThrown[JsMatchException](
      ("object:1:0", Some(JsObject(Seq()))),
      ("object:1000:500", Some(JsObject(Seq())))
    )
  }
}

class LowerBoundedObjectProcessorSpec extends ProcessorSpec {

  override val processor = LowerBoundedObjectProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("object", maybeJsValue),
      ("object::", maybeJsValue),
      ("object:-3:", maybeJsValue),
      ("object:1.5:", maybeJsValue),
      ("object:5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsObject of correct size" in {
    assertAllMatchSuccess(
      ("object:0:", Some(JsObject(Seq()))),
      ("object:1:", Some(JsObject(Seq(("z", JsNumber(9)))))),
      ("object:0:", Some(JsObject(Seq(("z", JsNumber(9)))))),
      ("object:5:", Some(JsObject(Seq.fill(5)((Random.nextInt().toString, JsNull))))),
      ("object:5:", Some(JsObject(Seq.fill(9)((Random.nextInt().toString, JsNull)))))
    )
  }

  it should "fail with relevant pattern and not JsObject" in {
    assertAllMatchError(
      ("object:0:", None),
      ("object:5:", Some(JsNull)),
      ("object:0:", Some(JsBoolean(false))),
      ("object:100:", Some(JsNumber(0))),
      ("object:2:", Some(JsArray(Seq()))),
      ("object:10:", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsObject of wrong size" in {
    assertAllMatchError(
      ("object:1:", Some(JsObject(Seq()))),
      (s"object:${Int.MaxValue}:", Some(JsObject(Seq()))),
      ("object:6:", Some(JsObject(Seq.fill(5)((Random.nextInt().toString, JsNull))))),
      ("object:15:", Some(JsObject(Seq.fill(5)((Random.nextInt().toString, JsNull))))),
      ("object:60:", Some(JsObject(Seq.fill(5)((Random.nextInt().toString, JsNull)))))
    )
  }
}

class UpperBoundedObjectProcessorSpec extends ProcessorSpec {

  override val processor = UpperBoundedObjectProcessor

  "match" should "be skipped with irrelevant patterns" in {
    val maybeJsValue = Some(JsNull)

    assertAllMatchSkip(
      (null, maybeJsValue),
      ("", maybeJsValue),
      ("a", maybeJsValue),
      ("object", maybeJsValue),
      ("object::", maybeJsValue),
      ("object::-3", maybeJsValue),
      ("object::1.5", maybeJsValue),
      ("object:5", maybeJsValue)
    )
  }

  it should "succeed with relevant pattern and JsObject of correct size" in {
    assertAllMatchSuccess(
      ("object::0", Some(JsObject(Seq()))),
      ("object::1", Some(JsObject(Seq(("z", JsNumber(9)))))),
      ("object::0", Some(JsObject(Seq()))),
      ("object::5", Some(JsObject(Seq.fill(5)((Random.nextInt().toString, JsNull))))),
      ("object::5", Some(JsObject(Seq.fill(3)((Random.nextInt().toString, JsNull)))))
    )
  }

  it should "fail with relevant pattern and not JsObject" in {
    assertAllMatchError(
      ("object::0", None),
      ("object::5", Some(JsNull)),
      ("object::0", Some(JsBoolean(false))),
      ("object::100", Some(JsNumber(0))),
      (s"object::${Int.MaxValue}", Some(JsArray(Seq()))),
      ("object::10", Some(JsString("")))
    )
  }

  it should "fail with relevant pattern and JsObject of wrong size" in {
    assertAllMatchError(
      ("object::0", Some(JsObject(Seq(("z", JsNumber(9)))))),
      ("object::6", Some(JsObject(Seq.fill(7)((Random.nextInt().toString, JsNull))))),
      ("object::15", Some(JsObject(Seq.fill(20)((Random.nextInt().toString, JsNull)))))
    )
  }
}
