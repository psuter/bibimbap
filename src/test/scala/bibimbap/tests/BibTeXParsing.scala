package bibimbap.tests

import bibimbap.data._
import bibimbap.bibtex._

import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BibTeXParsing extends FunSuite with ShouldMatchers {
  def entriesAndErrors(str : String) : (Seq[BibTeXEntry],Int) = {
    var errorCount : Int = 0
    def errorHandler(s : String) : Unit = {
      errorCount += 1
      println(s)
    }

    val parser = new BibTeXParser(Source.fromString(str), errorHandler)
    val entries = parser.entries.toSeq
    for(entry <- entries) println(entry)
    (entries, errorCount)
  }

  test("Two valid entries") {
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
    
    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (2)
    errors should equal (0)
  }

  test("One valid entry with one broken field") {
    val src = """
@inproceedings{ThreeGuys1291OnTheMountain,
  title  = "On " # "BibTeX {"}Entries{"}",
  author = {Alfred U. Thor},
  field = "Broken String,
  other = 1,
  year   = 2012,
  booktitle = {{BIG}CONF}
} 
    """
    
    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (1)
    errors should equal (1)
  }
  
}
