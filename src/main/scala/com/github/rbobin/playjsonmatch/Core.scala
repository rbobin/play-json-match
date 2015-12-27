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
    compareJsValues(pattern, Some(actual), Seq("/")) match {
      case NO_ERRORS => true
      case _: List[_] => throw new RuntimeException() // TODO descriptive error message
    }

  private def compareJsValues(expected: JsValue, maybeActual: Option[JsValue], path: JsPath): Errors = expected match {
    case x: JsArray => compareJsArrays(x, maybeActual, path)
    case x: JsString => compareJsStrings(x, maybeActual, path)
    case x: JsObject => compareJsObjects(x, maybeActual, path)
    case x: JsNumber => compareJsNumbers(x, maybeActual, path)
    case x: JsBoolean => compareJsBoolean(x, maybeActual, path)
    case JsNull => compareJsNull(maybeActual, path)
  }

  private def compareJsArrays(expected: JsArray, maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(actual: JsArray) if expected.value.size != actual.value.size =>
        arraysDifferentSizeError(expected, actual, path)
      case Some(actual: JsArray) => (expected.value, actual.value, expected.value.indices)
        .zipped
        .flatMap { (patternElement, actualElement, index) =>
          compareJsValues(patternElement, Some(actualElement), path :+ s"[$index]")
        }
      case Some(x) => typeMismatchError(ARRAY, x, path)
      case None => missingElementError(ARRAY, path)
    }

  private def compareJsObjects(expected: JsObject, maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(actual: JsObject) =>
        def compareIntersection: Errors = expected
          .keys
          .intersect(actual.keys)
          .flatMap { (key: String) =>
            compareJsValues(expected.value(key), actual.value.get(key), path :+ key)
          }
          .toSeq
        def compareSubset: Errors = expected
          .keys
          .diff(actual.keys)
          .flatMap { (key: String) =>
            compareJsValues(expected.value(key), None, path :+ key)
          }
          .toSeq
        def compareSuperset: Errors = objectSupersetError(actual.keys.diff(expected.keys).toSeq, path)

        compareIntersection ++ compareSubset ++ compareSuperset
      case Some(x) => typeMismatchError(OBJECT, x, path)
      case None => missingElementError(OBJECT, path)
    }

  private def compareJsStrings(expected: JsString, maybeActual: Option[JsValue], path: JsPath): Errors =
    expected.value match {
      case PATTERN(pattern) => ???
      case _ => maybeActual match {
        case Some(actual: JsString) if expected.value != actual.value =>
          equalityError(STRING, s"\'${expected.value}\'", s"\'${actual.value}\'", path)
        case Some(actual: JsString) => NO_ERRORS
        case Some(x) => typeMismatchError(STRING, x, path)
        case None => missingElementError(STRING, path)
      }
    }

  private def compareJsNumbers(expected: JsNumber, maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(actual: JsNumber) if expected.value != actual.value =>
        equalityError(NUMBER, expected.value.toString(), actual.value.toString(), path)
      case Some(actual: JsNumber) => NO_ERRORS
      case Some(x) => typeMismatchError(NUMBER, x, path)
      case None => missingElementError(NUMBER, path)
    }

  private def compareJsBoolean(expected: JsBoolean, maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(actual: JsBoolean) if expected.value != actual.value =>
        equalityError(BOOLEAN, expected.value.toString, actual.value.toString, path)
      case Some(actual: JsBoolean) => NO_ERRORS
      case Some(x) => typeMismatchError(BOOLEAN, x, path)
      case None => missingElementError(BOOLEAN, path)
    }

  private def compareJsNull(maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(JsNull) => NO_ERRORS
      case Some(x) => typeMismatchError(NULL, x, path)
      case None => missingElementError(NULL, path)
    }
}
