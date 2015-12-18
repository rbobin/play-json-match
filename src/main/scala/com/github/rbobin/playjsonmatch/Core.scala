package com.github.rbobin.playjsonmatch

import play.api.libs.json._
import com.github.rbobin.playjsonmatch.Errors._

object Core {

  val PATTERN = (
    "^" +        // Start of string
      "\\#\\[" + // Start of pattern
      "(.*)" +   // Capturing group
      "\\]" +    // End of pattern
      "$"        // End of string
    ).r

  def matches(pattern: JsValue, actual: JsValue): Boolean =
    compareJsValues(pattern, Some(actual)) match {
      case Nil => true
      case _: List[_] => throw new RuntimeException() // TODO descriptive error message
    }

  private def compareJsValues(expected: JsValue, maybeActual: Option[JsValue]): Seq[String] = expected match {
    case x: JsArray => compareJsArrays(x, maybeActual)
    case x: JsObject => compareJsObjects(x, maybeActual)
    case x: JsString => compareJsStrings(x, maybeActual)
    case x: JsNumber => compareJsNumbers(x, maybeActual)
    case x: JsBoolean => compareJsBoolean(x, maybeActual)
    case JsNull => compareJsNull(maybeActual)
  }

  private def compareJsArrays(expected: JsArray, maybeActual: Option[JsValue]): Seq[String] = maybeActual match {
    case Some(actual: JsArray) if expected.value.size != actual.value.size => arraysDifferentSizeError
    case Some(actual: JsArray) => (expected.value, actual.value)
      .zipped
      .flatMap { (patternElement, actualElement) =>
        compareJsValues(patternElement, Some(actualElement))
      }
    case Some(_) => typeMismatchError
    case None => missingElementError
  }

  private def compareJsObjects(expected: JsObject, maybeActual: Option[JsValue]): Seq[String] = maybeActual match {
    case Some(actual: JsObject) =>
      def compareIntersection: Seq[String] = expected
        .keys
        .intersect(actual.keys)
        .flatMap { (key: String) =>
          compareJsValues(expected.value(key), actual.value.get(key))
        }
        .toSeq
      def compareSubset: Seq[String] = expected
        .keys
        .diff(actual.keys)
        .flatMap { (key: String) =>
          compareJsValues(expected.value(key), None)
        }
        .toSeq
      def compareSuperset: Seq[String] = objectSupersetError(actual.keys.diff(expected.keys).toSet)

      compareIntersection ++ compareSubset ++ compareSuperset
    case Some(_) => typeMismatchError
    case None => missingElementError
  }

  private def compareJsStrings(expected: JsString, maybeActual: Option[JsValue]): Seq[String] = expected.value match {
    case PATTERN(pattern) => ???
    case _ => maybeActual match {
      case Some(actual: JsString) if expected.value != actual.value => equalityError
      case Some(actual: JsString) => Nil
      case Some(_) => typeMismatchError
      case None => missingElementError
    }
  }

  private def compareJsNumbers(expected: JsNumber, maybeActual: Option[JsValue]): Seq[String] = maybeActual match {
    case Some(actual: JsNumber) if expected.value != actual.value => equalityError
    case Some(actual: JsNumber) => Nil
    case Some(_) => typeMismatchError
    case None => missingElementError
  }

  private def compareJsBoolean(expected: JsBoolean, maybeActual: Option[JsValue]): Seq[String] = maybeActual match {
    case Some(actual: JsBoolean) if expected.value != actual.value => equalityError
    case Some(actual: JsBoolean) => Nil
    case Some(_) => typeMismatchError
    case None => missingElementError
  }

  private def compareJsNull(maybeActual: Option[JsValue]): Seq[String] = maybeActual match {
    case Some(JsNull) => Nil
    case Some(_) => typeMismatchError
    case None => missingElementError
  }
}
