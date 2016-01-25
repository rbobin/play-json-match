package com.github.rbobin.playjsonmatch

import com.github.rbobin.playjsonmatch.utils.Prettifier

/**
  * Grab a resource intended for use in a failure message. For each argument passed,
  * convert it to a string by calling decorateToStringValue, which will do things such
  * as highlight differences in two strings that were supposd to be equal.
  */
object FailureMessages {

  def apply(resourceName: String): String = Resources(resourceName)
  def apply(resourceName: String, args: Any*): String =
    Resources(resourceName, args.map((arg: Any) => decorateToStringValue(arg)): _*)

  def decorateToStringValue(o: Any): String = Prettifier.default(o)
}
