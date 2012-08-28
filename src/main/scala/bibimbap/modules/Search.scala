package bibimbap
package modules

import akka.actor._
import java.util.concurrent.TimeoutException

import data._

class Search(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "search"

  override val dependsOn = Set("results")

  private val searchModules = List(
    context.actorOf(Props(new SearchLocal(repl, console, settings)), name = "SearchLocal"),
    context.actorOf(Props(new SearchDBLP(repl, console, settings)),  name = "SearchDBLP")
  )

  lazy val resultsModule = modules("results")

  override def receive: Receive = {
    case CommandL("search", args) =>
      val results = doSearch(args)

      syncCommand(resultsModule, SearchResults(results))

      sender ! CommandSuccess

    case ImportedResult(res) =>
      for (m <- searchModules) {
        m ! ImportedResult(res)
      }

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
        console ! Error("Failed to gather search results in time")
        Nil
    }
  }

  private def combineResults(resultss: List[SearchResults]): List[SearchResult]= {
    resultss.flatMap(_.entries).groupBy(_.entry.getKey).values.map(res => SearchResult(res.head.entry, res.head.link, res.flatMap(_.sources).toSet)).toList
  }

  val helpItems = Map(
    "search" -> HelpEntry("search <terms..>", "Searches for <terms> using the various search modules")
  )
}
