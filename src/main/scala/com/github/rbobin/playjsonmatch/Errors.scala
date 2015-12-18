package com.github.rbobin.playjsonmatch

private[playjsonmatch] object Errors {

  def missingElementError: Seq[String] = ???

  def typeMismatchError: Seq[String] = ???

  def arraysDifferentSizeError: Seq[String] = ???

  def objectSupersetError(keys: Set[String]): Seq[String] = ???

  def equalityError: Seq[String] = ???

}
