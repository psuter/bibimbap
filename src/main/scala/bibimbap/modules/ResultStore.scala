package bibimbap
package modules

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.util.concurrent.{Executors, TimeoutException}
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.util.duration._
import scala.concurrent.ExecutionContext

import data._

class ResultStore(val repl: ActorRef, val logger: ActorRef, val settings: Settings) extends Module {
  val name = "results"

  private var results = List[SearchResult]()

  override def receive: Receive = {
    case Command1("list") | Command1("show") =>
      displayResults()
      sender ! CommandSuccess
    case Command2("import", ind) =>
      getResults(ind) match {
        case Some(rs) =>
          for (r <- rs) {
            doImport(r)
          }
        case None =>
          logger ! Error("Invalid search result")
      }
      sender ! CommandSuccess
    case Command2("show", ind) =>
      getResults(ind) match {
        case Some(rs) =>
          for (r <- rs) {
            doShow(r)
          }
        case None =>
          logger ! Error("Invalid search result")
      }
      sender ! CommandSuccess

    case SearchResults(newResults) =>
      results = newResults
      displayResults()

      sender ! CommandSuccess

    case GetResults(index) =>
      sender ! SearchResults(getResults(index).getOrElse(Nil))

    case ShowResults =>
      displayResults()
      sender ! CommandSuccess

    case x =>
      super.receive(x)
  }

  private def getResults(index: String): Option[List[SearchResult]] = {
    try {
      val i = index.toInt
      if (i < results.size && i >= 0) {
        Some(List(results(i)))
      } else {
        None
      }
    } catch {
      case _: Throwable =>
        None
    }
  }

  private def doImport(res: SearchResult) {
    import java.io.{FileWriter,File}

    val fn = settings("general", "bib.filename")
    val fw = new FileWriter(new File(fn), true)
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
        logger ! Warning("Could not store in clipboard: "+e.getMessage.trim)
    }

    logger ! Success("Imported: \\cite{"+res.entry.getKey+"}")
  }

  private def doShow(res: SearchResult) {
    logger ! Out(res.entry.toString)
  }

  private def displayResults() {
    var i = 0
    for (res <- results) {
      val spc = if (i < 10) "" else " "
      logger ! Info(spc+"["+i+"] "+res.entry.inlineString)
      i += 1
    }
    if (results.isEmpty) {
      logger ! Info("No match")
    }
  }

  val helpItems = Map(
    "list"   -> HelpEntry("list",             "Displays the current list of results."),
    "import" -> HelpEntry("import <result>",  "Imports the <result>th item from the last search results into managed.bib"),
    "show"   -> HelpEntry("show <result>",    "Displays the bib entry for the <results>th search result.")
  )
}
