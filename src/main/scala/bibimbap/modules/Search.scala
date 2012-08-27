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

  lazy val resultsModule = modules("results")

  override def receive: Receive = {
    case CommandL("search", args) =>
      val results = doSearch(args)

      syncCommand(resultsModule, SearchResults(results))

      sender ! CommandSuccess

    case Search(terms) =>
      val results = doSearch(terms)
      sender ! SearchResults(results)

    case SearchOne(terms) =>
      val results = doSearch(terms)
      sender ! SearchResults(results.headOption.toList)

    case x =>
      super.receive(x)
  }

  private def doSearch(args: List[String]): List[SearchResult] = {
    try {
      val resultsPerSearch = dispatchCommand[SearchResults](Search(args), searchModules)
      combineResults(resultsPerSearch)
    } catch {
      case e: TimeoutException =>
        logger ! Error("Failed to gather search results in time")
        Nil
    }
  }

  private def combineResults(resultss: List[SearchResults]): List[SearchResult]= {
    resultss.flatMap(_.entries).groupBy(_.entry.getKey).values.map(_.head).toList
  }

  val helpItems = Map(
    "search" -> HelpEntry("search <terms..>", "Searches for <terms> using the various search modules")
  )
}
