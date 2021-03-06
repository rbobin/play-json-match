package com.github.rbobin.playjsonmatch

import play.api.libs.json.Json

class CoreSpec extends UnitSpec {

  val testJson =
    """
      | {
      |   "string": "test",
      |   "number": 1000,
      |   "object": {
      |     "boolean": true,
      |     "null": null
      |   },
      |   "array": [1, 2, 3]
      | }
    """.stripMargin

  "Json" should "match itself" in {
    val jsValue = Json.parse(testJson)
    Core.matches(jsValue, jsValue) should be(true)
  }


}
