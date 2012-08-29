package bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import bibtex._

import scala.io.Source
import java.io.{File, FileWriter}
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

class Managed(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module
                                                                                    with LuceneRAMBackend
                                                                                    with LuceneSearchProvider {
  val name = "Managed"

  val source = "managed"

  val managedPath = settings("general", "bib.filename")
  var lastModified = 0l
  val managedFile = new File(managedPath)

  def loadFile() {
    if(managedFile.exists && managedFile.isFile && managedFile.canRead) {

      lastModified = managedFile.lastModified()

      val parser = new BibTeXParser(Source.fromFile(managedFile), console ! Error(_))
      for (entry <- parser.entries) {
        addEntry(entry)
      }
    }
  }

  override def receive: Receive = {
    case msg @ OnStartup(os) => {
      super[Module].receive(msg)
      loadFile()
    }

    case Search(terms) =>
      sender ! search(terms)

    case Command2("import", ind) =>
      syncMessage[SearchResults](modules("results"), GetResults(ind)) match {
        case Some(SearchResults(rs)) =>
          for (r <- rs) {
            doImport(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandSuccess

    case ImportedResult(res) =>
      // NOOP: we sent this.

    case msg =>
      super[Module].receive(msg)
  }

  private def doImport(res: SearchResult) {

    def displayImported() {
      try {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val stringSel = new StringSelection(res.entry.getKey)
        clipboard.setContents(stringSel, stringSel)
      } catch {
        case e: java.awt.HeadlessException =>
          console ! Warning("Could not store in clipboard: "+e.getMessage.trim)
      }

      console ! Success("Imported: \\cite{"+res.entry.getKey+"}")
    }

    if (!res.sources.contains("managed") || res.sources.contains("modified")) {
      var action = "import"

      if (lastModified > 0 && lastModified < managedFile.lastModified) {
        console ! Warning("Managed file has been modified in the meantime!")
        var ask = true
        while(ask) {
          syncMessage[LineRead](console, ReadLineWithHandle("(i)mport (c)ancel (r)eload> ")) match {
            case Some(LineRead("c")) =>
              ask = false
              action = "cancel"
            case Some(LineRead("i")) =>
              ask = false
              action = "import"
            case Some(LineRead("r")) =>
              ask = false
              action = "reload"
            case Some(LineRead(_)) =>
              console ! Error("wat?")
              ask = true
            case _ =>
              action = "cancel"
              ask = false
          }
        }
      }

      if (action == "import") {
        addEntry(res.entry)

        import org.apache.lucene.index.IndexReader
        val reader = IndexReader.open(index)

        val entries = for (i <- 0 until reader.maxDoc if !reader.isDeleted(i); entry <- documentToEntry(reader.document(i))) yield  entry

        val fw = new FileWriter(managedFile, false)

        for (entry <- entries.sortBy(_.getKey)) {
          fw.write(entry.toString)
          fw.write("\n\n")
        }

        fw.close

        // Inform search module that we imported this
        modules("search") ! ImportedResult(res)

        displayImported()
      } else if(action == "cancel") {
        console ! Success("Aborted")
      } else if (action == "reload") {
        loadFile()
        console ! Success("File reloaded!")
      }
    } else {
      console ! Success("Entry already imported as is!")
      displayImported()
    }
  }

  val helpItems = Map(
    "import" -> HelpEntry("import <result>",  "Imports the <result>th item from the last search results into managed bib file")
  )
}
