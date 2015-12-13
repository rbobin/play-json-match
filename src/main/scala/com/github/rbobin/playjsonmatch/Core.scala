package com.github.rbobin.playjsonmatch

import play.api.libs.json._

object Core {

  def matches(pattern: JsValue, actual: JsValue): Boolean =
    compareJsValues(pattern, Some(actual)) match {
      case Nil => true
      case _: List[_] => throw new RuntimeException() // TODO descriptive error message
    }

  private def missingElementError: List[String] = ???

  private def typeMismatchError: List[String] = ???

  private def arraysDifferentSizeError: List[String] = ???

  private def compareJsValues(pattern: JsValue, actual: Option[JsValue]): Seq[String] = pattern match {
    case x: JsArray => compareJsArrays(x, actual)
    case x: JsObject => compareJsObjects(x, actual)
    case x: JsString => compareJsStrings(x, actual)
    case x: JsNumber => compareJsNumbers(x, actual)
    case x: JsBoolean => compareJsBoolean(x, actual)
    case JsNull => compareJsNull(actual)
  }

  private def compareJsArrays(pattern: JsArray, maybeActual: Option[JsValue]): Seq[String] = maybeActual match {
    case Some(actual: JsArray) if pattern.value.size != actual.value.size => arraysDifferentSizeError
    case Some(actual: JsArray) => (pattern.value, actual.value)
      .zipped
      .flatMap { (patternElement, actualElement) =>
        compareJsValues(patternElement, Some(actualElement))
      }
    case Some(_) => typeMismatchError
    case None => missingElementError
  }

  private def compareJsObjects(pattern: JsObject, actual: Option[JsValue]): Seq[String] = actual match {
    case Some(x: JsObject) =>
      def compareIntersection: Seq[String] = Seq()[String]
      def compareSubset: Seq[String] = Seq()[String]
      def compareSuperset: Seq[String] = Seq()[String]

      compareIntersection ++ compareSubset ++ compareSuperset
    case Some(_) => typeMismatchError
    case None => missingElementError
  }

  private def compareJsStrings(pattern: JsString, actual: Option[JsValue]): List[String] = pattern.value match {
    case "*" => actual match {
      case None => "Missing value" :: errors
      case Some(y: JsString) => errors
      case Some(_) => "Not a string" :: errors
    }
    case x => actual match {
      case None => "Missing string" :: errors
      case Some(y: JsString) => if (x != y.value)
        "String not equal" :: errors
      else
        errors
      case Some(_) => "Not a string" :: errors
    }
  }

  private def compareJsNumbers(pattern: JsNumber, actual: Option[JsValue]): List[String] = actual match {
    case None => "Missing number" :: errors
    case Some(x: JsNumber) => if (pattern != x)
      "Numbers not equal" :: errors
    else
      errors
    case Some(_) => "Not a number" :: errors
  }

  private def compareJsBoolean(pattern: JsBoolean, actual: Option[JsValue]): List[String] = actual match {
    case None => "Missing boolean" :: errors
    case Some(x: JsBoolean) => if (pattern != x)
      "Boolean not equal" :: errors
    else
      errors
    case Some(_) => "Not a boolean" :: errors
  }

  private def compareJsNull(actual: Option[JsValue]): List[String] = actual match {
    case Some(JsNull) => errors
    case Some(_) => "Not a null" :: errors
    case None => "Missing null" :: errors
  }

}
