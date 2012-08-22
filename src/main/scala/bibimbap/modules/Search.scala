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

class Search(val repl: ActorRef, val logger: ActorRef, val settings: Settings) extends Module {
  val name = "Search"

  var lastSearch = List[SearchResult]()

  val searchModules = List(
    context.actorOf(Props(new SearchLocal(repl, logger, settings)), name = "SearchLocal"),
    context.actorOf(Props(new SearchDBLP(repl, logger, settings)),  name = "SearchDBLP")
  )

  def receive = {
    case Command(Command1("clear")) =>
      dispatch(Clear)
      sender ! CommandSuccess
    case Command(CommandL("search", args)) =>
      doSearch(args)
      sender ! CommandSuccess
    case Command(Command2("import", ind)) =>
      getSearchResult(ind) match {
        case Some(sr) =>
          doImport(sr)
        case None =>
          logger ! Error("Invalid search result")
      }
      sender ! CommandSuccess
    case Command(Command2("show", ind)) =>
      getSearchResult(ind) match {
        case Some(sr) =>
          doShow(sr)
        case None =>
          logger ! Error("Invalid search result")
      }
      sender ! CommandSuccess
    case _ =>
      sender ! CommandUnknown
  }

  def dispatch(msg: Any) {
    for (m <- searchModules) {
      m ! msg
    }
  }

  def doSearch(args: List[String]) = {
    val futures = searchModules.map(actor => (actor ? Search(args)).mapTo[SearchResults] recover {
      case e: Throwable =>
        logger ! Error(e.getMessage)
        Nil
    })

    try {
      val resultsPerSearch = Await.result(Future.sequence(futures), 15.seconds)
      val results = combineResults(resultsPerSearch)

      lastSearch = results

      displayResults(lastSearch)

    } catch {
      case e: TimeoutException =>
        logger ! Error("Failed to gather search results in time")
    }
  }

  def doImport(res: SearchResult) {
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

    dispatch(Store(List(res)))

    logger ! Success("Imported: \\cite{"+res.entry.getKey+"}")
  }

  def doShow(res: SearchResult) {
    logger ! Out(res.entry.toString)
  }

  def getSearchResult(index: String): Option[SearchResult] = {
    try {
      val i = index.toInt
      if (i < lastSearch.size && i >= 0) {
        Some(lastSearch(i))
      } else {
        None
      }
    } catch {
      case _: Throwable =>
        None
    }
  }

  private def combineResults(results : List[SearchResults]): SearchResults = {
    results.flatten.groupBy(_.entry.getKey).values.map(_.head).toList
  }

  private def displayResults(results: SearchResults) {
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
}
