package com.github.rbobin.playjsonmatch.utils

case class MalformedJsonPatternException(message: String) extends RuntimeException
case class MultipleMatchException(message: String) extends RuntimeException
