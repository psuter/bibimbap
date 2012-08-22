package bibimbap.tests

import bibimbap.json._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class JsonUsage extends FunSuite with ShouldMatchers {
  private def parsesAs(in : String, v : JValue) {
    (new JsonParser).parse(in) should equal (v)
  }

  test("Base cases") {
    parsesAs("""null""", JNull)

    parsesAs("""42""", JInt(42))
  }
}
