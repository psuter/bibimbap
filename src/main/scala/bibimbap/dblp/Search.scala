package bibimbap
package dblp

import bibimbap.data._

import org.squeryl.PrimitiveTypeMode._

trait Search extends Export {
  val settings : Settings

  private val db = DB(settings, false)

  def search(string : String) : SearchResult = {
    import MySQLMatchAgainst._

    def results = 
      from(db.entries)(e => where(
        matchAgainst(e.allAuthors, e.allEditors, e.title, e.booktitle)(string) > 0.0f
        and
        (e.kind === "inproceedings" or e.kind === "article")
      ) select(e))

    inTransaction {
      results.take(20).flatMap(export(_))
    }
  }

  def fullEntry(dblpkey : String) : Entry = {
    inTransaction {
      from(db.entries)(e => where(e.key === dblpkey) select(e)).head
    }
  }
}
