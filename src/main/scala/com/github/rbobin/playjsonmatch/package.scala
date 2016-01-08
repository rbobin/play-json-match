package com.github.rbobin

package object playjsonmatch {

  type Errors = Seq[String]
  type JsPath = Seq[String]

  val NO_ERRORS = Nil
  val ARRAY = "Array"
  val OBJECT = "Object"
  val STRING = "String"
  val NUMBER = "Number"
  val BOOLEAN = "Boolean"
  val NULL = "Null"
  val NONE = "Nothing"
}