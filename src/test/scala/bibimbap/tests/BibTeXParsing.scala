package bibimbap.tests

import bibimbap.bibtex._

import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BibTeXParsing extends FunSuite with ShouldMatchers {
  test("Basic BibTeX parsing") {

    val src = """

This is a dummy BibTeX file.

@inproceedings{Thor2012OnEntries,
  title  = "On " # "BibTeX {"}Entries{"}",
  author = {Alfred U. Thor},
  year   = 2012,
  booktitle = {{BIG}CONF}
} @article{ ThisKeyIsDroppedByBibimbap ,
  title={All {CAPS}, A Keyboard Memoir},
  author = "Quentin Werty", year=1976, journal="Keystroke Prenvention" }
    """

    val parser = new BibTeXParser(Source.fromString(src))
    val entries = parser.entries.toList
    val count = entries.size
    //for(entry <- entries) println(entry)
    count should equal (2)
  }
}
