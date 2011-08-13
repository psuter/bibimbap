package bibimbap
package dblp

import org.squeryl.PrimitiveTypeMode._

class Search(settings : Settings) {
  private val db = DB(settings, false)

  def search(string : String) : Unit = {
    def results = 
      from(db.persons)(p => where(p.name like ("%" + string + "%")) select(p))

    def joined = join(
      from(db.persons)(p => where(p.name like ("%" + string + "%")) select(p)),
      db.authorship,
      db.entries)((p, as, e) =>
      select(p, as, e)
      on(p.id === as.authorId, as.entryId === e.id))

    def results2 =
      from(joined)(triple => select(triple._1, triple._3))

    println("Names : ")
    inTransaction {
      for(p <- results.take(5)) {
        println(p.name + " (" + p.id + ")")
      }
    }

    println("")
    println("Papers authored : ")
    inTransaction {
      for((p, e) <- results2) {
        println(p.name + " --> " + e.year + " " + e.title)
      }
    }
  }
}
