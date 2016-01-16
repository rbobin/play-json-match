package com.github.rbobin.playjsonmatch

class ErrorsSpec extends UnitSpec {

  val testPath = Seq("root", "leaf")

  "objectSupersetError" should "return empty list when invoked with empty seq" in {
    val keys = Seq()
    val result = Errors.objectSupersetError(keys, testPath)

    result should be (Nil)
  }

  ignore should "return sequence of one error message when invoked with non-empty seq" in {
    val keys = Seq("key1", "key2", "key3")
    val result = Errors.objectSupersetError(keys, testPath)

    result should be (Seq())
  }

}
