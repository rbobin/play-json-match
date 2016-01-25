package com.github.rbobin.playjsonmatch

import java.text.MessageFormat
import java.util.ResourceBundle

/**
  * Resources for internationalization.
  */
private[playjsonmatch] object Resources {

  lazy val resourceBundle = ResourceBundle.getBundle("com.github.rbobin.playjsonmatch.JsonMatchBundle")

  def apply(resourceName: String): String = resourceBundle.getString(resourceName)
  def apply(resourceName: String, argArray: AnyRef*): String = {
    val raw = apply(resourceName)
    val msgFmt = new MessageFormat(raw)
    msgFmt.format(argArray)
  }
}
