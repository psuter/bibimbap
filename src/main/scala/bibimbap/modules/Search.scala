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
  val name = "search"

  override val dependsOn = Set("results")

  private val searchModules = List(
    context.actorOf(Props(new SearchLocal(repl, logger, settings)), name = "SearchLocal"),
    context.actorOf(Props(new SearchDBLP(repl, logger, settings)),  name = "SearchDBLP")
  )

  override def receive: Receive = {
    case CommandL("search", args) =>
      doSearch(args)
      sender ! CommandSuccess
    case x =>
      super.receive(x)
  }

  private def doSearch(args: List[String]) = {
    try {
      val resultsPerSearch = dispatchCommand[SearchResults](Search(args), searchModules)
      val results = combineResults(resultsPerSearch)

      syncCommand(modules("results"), StoreResults(results))
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
