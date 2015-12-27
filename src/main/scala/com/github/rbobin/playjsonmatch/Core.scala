package com.github.rbobin.playjsonmatch

import play.api.libs.json._
import com.github.rbobin.playjsonmatch.Errors._

object Core {

  val PATTERN = (
    "^" + // Start of string
      "\\#\\[" + // Start of pattern
      "(.*)" + // Capturing group
      "\\]" + // End of pattern
      "$" // End of string
    ).r

  def matches(pattern: JsValue, actual: JsValue): Boolean =
    compareJsValues(pattern, Some(actual), Seq("/")) match {
      case Nil => true
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
      case Some(x) => typeMismatchError(JsArray.getClass, x.getClass, path)
      case None => missingElementError(JsArray.getClass, path)
    }

  private def compareJsObjects(expected: JsObject, maybeActual: Option[JsValue], path: JsPath): Errors = maybeActual match {
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
    case Some(x) => typeMismatchError(JsObject.getClass, x.getClass, path)
    case None => missingElementError(JsObject.getClass, path)
  }

  private def compareJsStrings(expected: JsString, maybeActual: Option[JsValue], path: JsPath): Errors =
    expected.value match {
      case PATTERN(pattern) => ???
      case _ => maybeActual match {
        case Some(actual: JsString) if expected.value != actual.value =>
          equalityError(expected.getClass, s"\'${expected.value}\'", s"\'${actual.value}\'", path)
        case Some(actual: JsString) => Nil
        case Some(x) => typeMismatchError(expected.getClass, x.getClass, path)
        case None => missingElementError(expected.getClass, path)
      }
    }

  private def compareJsNumbers(expected: JsNumber, maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(actual: JsNumber) if expected.value != actual.value =>
        equalityError(expected.getClass, expected.value.toString(), actual.value.toString(), path)
      case Some(actual: JsNumber) => Nil
      case Some(x) => typeMismatchError(expected.getClass, x.getClass, path)
      case None => missingElementError(expected.getClass, path)
    }

  private def compareJsBoolean(expected: JsBoolean, maybeActual: Option[JsValue], path: JsPath): Errors =
    maybeActual match {
      case Some(actual: JsBoolean) if expected.value != actual.value =>
        equalityError(expected.getClass, expected.value.toString, actual.value.toString, path)
      case Some(actual: JsBoolean) => Nil
      case Some(x) => typeMismatchError(expected.getClass, x.getClass, path)
      case None => missingElementError(expected.getClass, path)
    }

  private def compareJsNull(maybeActual: Option[JsValue], path: JsPath): Errors = maybeActual match {
    case Some(JsNull) => Nil
    case Some(x) => typeMismatchError(JsNull.getClass, x.getClass, path)
    case None => missingElementError(JsNull.getClass, path)
  }
}
