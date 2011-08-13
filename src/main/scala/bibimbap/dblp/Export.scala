package bibimbap
package dblp

import bibimbap.data._

class Export(settings : Settings) {
  val db = DB(settings, false)

  def export(entry : Entry) : Option[BibTeXEntry] = entry.kind match {
    case "inproceedings" => {
      //val authors = db.persons.
      // select au.authorId, au.authorOrder, p.id, p.name FROM (SELECT * FROM Authorship where entryId = 49918) as au INNER JOIN Person AS p ON p.id = au.authorId order by authorOrder;

      None
    }
    case _ => None
  }
}
