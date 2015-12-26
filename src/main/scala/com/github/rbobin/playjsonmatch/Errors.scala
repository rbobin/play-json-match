package com.github.rbobin.playjsonmatch

import play.api.libs.json.JsArray

private[playjsonmatch] object Errors {

  def missingElementError[A](expected: Class[A], path: Seq[String]): Seq[String] = ???

  def typeMismatchError[A, B](expected: Class[A], actual: Class[B], path: Seq[String]): Seq[String] = ???

  def arraysDifferentSizeError(expected: JsArray, actual: JsArray, path: Seq[String]): Seq[String] = ???

  def objectSupersetError(keys: Set[String]): Seq[String] = keys.headOption.map { _ =>
    keys.map(_ => "").toSeq
  }.getOrElse(Nil)

  def equalityError[A](clazz: Class[A], expectedValue: String, actualValue: String, path: Seq[String]): Seq[String] = ???

}
