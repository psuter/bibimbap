package bibimbap
package dblp

import bibimbap.data._

class DBLPModule(settings : Settings) extends Module(settings) with Search {
  val name = "Local DBLP access"

  val keyword = "dblp"

  override val requiredSettings = List("db.type", "db.dsn", "db.username", "db.password")

  override def searchAction = Some(realSearchAction)

  val realSearchAction = new Action[SearchResult]("search") {
    val description = "Search for records in DBLP."

    def run(args : String*) : SearchResult = {
      search(args.mkString(" "))
    }
  }

  val importAction = new Action[Unit]("reimport") {
    val description = "Delete all data and reimport from dblp.xml."

    def run(args : String*) : Unit = {
      val importer = new XMLImporter("./dblp-local/dblp.xml", settings)
      importer.importEntries
    }
  }

  val createTablesAction = new Action[Unit]("init") {
    val description = "Attempt to create the DB schema."

    def run(args : String*) : Unit = {
      val db = new DB(settings, true)
    }
  }


  override val moreActions = Seq(realSearchAction, importAction, createTablesAction)
}
