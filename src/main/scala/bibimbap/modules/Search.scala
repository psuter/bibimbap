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

  private val searchModules = List(
    context.actorOf(Props(new SearchLocal(repl, logger, settings)), name = "SearchLocal"),
    context.actorOf(Props(new SearchDBLP(repl, logger, settings)),  name = "SearchDBLP")
  )

  override def receive: Receive = {
    case CommandL("search", args) =>
      doSearch(args)
    case _ =>
      super.receive
  }

  private def doSearch(args: List[String]) = {
    val futures = searchModules.map(actor => (actor ? Search(args)).mapTo[SearchResults] recover {
      case e: Throwable =>
        logger ! Error(e.getMessage)
        Nil
    })

    try {
      val resultsPerSearch = Await.result(Future.sequence(futures), 15.seconds)
      val results = combineResults(resultsPerSearch)

    } catch {
      case e: TimeoutException =>
        logger ! Error("Failed to gather search results in time")
    }
  }

  private def combineResults(results : List[SearchResults]): SearchResults = {
    results.flatten.groupBy(_.entry.getKey).values.map(_.head).toList
  }

  val helpItems = Map(
    "search" -> HelpEntry("search <terms..>", "Searches for <terms> using the various search modules")
  )
}
