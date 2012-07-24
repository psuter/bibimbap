package bibimbap.tests

import bibimbap.strings._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class StringConversions extends FunSuite with ShouldMatchers {
  test("Transliteration 1") {
    def produces(in : String, out : String) {
      MString.fromJava(in).toASCII should equal (out)
    }

    produces("Keyser Söze", "Keyser Soeze")
    produces("Lettøl from Ålborg", "Lettoel from AAlborg")
    produces("β-conversion", "beta-conversion")
    produces("élémentaire", "elementaire")
    produces("Warning ⚠ ", "Warning")
  }
}
