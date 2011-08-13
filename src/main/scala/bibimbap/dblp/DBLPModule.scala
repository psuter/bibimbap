package bibimbap
package dblp

import bibimbap.data._

class DBLPModule(settings : Settings) extends Module(settings) {
  val name = "Local DBLP access"

  val keyword = "dblp"

  override val requiredSettings = List("db.type", "db.dsn", "db.username", "db.password")

  val searchAction = new Action[Seq[SearchResult]] {
    val keyword = "search"
    val description = "Search for records in DBLP."

    def run(args : String*) : Seq[SearchResult] = {
      search.search(args.mkString(" "))
      Seq.empty
    }
  }

  val importAction = new Action[Unit] {
    val keyword = "reimport"
    val description = "Delete all data and reimport from dblp.xml."

    def run(args : String*) : Unit = {
      val importer = new XMLImporter("./dblp-local/dblp.xml", settings)
      importer.importEntries
    }
  }

  val createTablesAction = new Action[Unit] {
    val keyword = "init"
    val description = "Attempt to create the DB schema."

    def run(args : String*) : Unit = {
      val db = new DB(settings, true)
    }
  }

  private val search = new Search(settings)

  override val moreActions = Seq(searchAction, importAction, createTablesAction)
}
