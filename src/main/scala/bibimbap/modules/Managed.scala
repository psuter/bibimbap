package bibimbap
package modules

import akka.actor._
import data._
import strings._
import bibtex._

import scala.io.Source

class Managed(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module
                                                                                    with LuceneRAMBackend
                                                                                    with LuceneSearchProvider {
  val name = "Managed"

  val source = "managed"

  val managedPath = settings("general", "bib.filename")

  override def receive: Receive = {
    case msg @ OnStartup(os) =>
      super[Module].receive(msg)

      val parser = new BibTeXParser(Source.fromFile(managedPath), console ! Error(_))

      for (entry <- parser.entries) {
        addEntry(entry, None)
      }

    case Search(terms) =>
      sender ! search(terms)

    case Command2("import", ind) =>
      syncMessage[List[SearchResult]](modules("results"), GetResults(ind)) match {
        case Some(rs) =>
          for (r <- rs) {
            doImport(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandSuccess

    case ImportedResult(res) =>
      onImport(res)
      // no message back

    case msg =>
      super[Module].receive(msg)
  }

  private def doImport(res: SearchResult) {
    import java.io.{FileWriter,File}

    val fw = new FileWriter(new File(managedPath), true)
    fw.write(res.entry.toString)
    fw.write("\n\n")
    fw.close

    import java.awt.Toolkit
    import java.awt.datatransfer.StringSelection
    try {
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val stringSel = new StringSelection(res.entry.getKey)
      clipboard.setContents(stringSel, stringSel)
    } catch {
      case e: java.awt.HeadlessException =>
        console ! Warning("Could not store in clipboard: "+e.getMessage.trim)
    }

    // Inform search module that we imported this
    modules("search") ! ImportedResult(res)

    console ! Success("Imported: \\cite{"+res.entry.getKey+"}")
  }

  val helpItems = Map(
    "import" -> HelpEntry("import <result>",  "Imports the <result>th item from the last search results into managed.bib")
  )
}
