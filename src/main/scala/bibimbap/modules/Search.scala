package bibimbap
package modules

import akka.actor._
import java.util.concurrent.TimeoutException

import bibtex._

class Search(val repl: ActorRef, val console: ActorRef, val settings: Settings, val searchProviders: List[ActorRef]) extends Module {
  val name = "search"

  override val dependsOn = Set("results")

  lazy val resultsModule = modules("results")

  override def receive: Receive = {
    case CommandL("search", args) =>
      val results = doSearch(args)

      syncCommand(resultsModule, SearchResults(results))

      sender ! CommandSuccess

    case ImportedResult(res) =>
      for (m <- searchProviders) {
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
      val resultsPerSearch = dispatchCommand[SearchResults](Search(args), searchProviders)
      combineResults(resultsPerSearch)
    } catch {
      case e: TimeoutException =>
        console ! Error("Failed to gather search results in time")
        Nil
    }
  }

  private def combineResults(resultss: List[SearchResults]): List[SearchResult]= {
    val groupedByEntry = resultss.flatMap(_.entries).groupBy(_.entry.getKey).values

    val combined = for (res <- groupedByEntry) yield {
      SearchResult(res.head.entry,
                   res.flatMap(_.sources).toSet,
                   res.map(_.relevance).max,
                   res.exists(_.isEdited),
                   res.exists(_.isManaged))
    }

    val sorted = combined.toList.sortBy(- _.relevance)

    sorted
  }

  val helpItems = Map(
    "search" -> HelpEntry("search <terms..>", "Searches for <terms> using the various search providers")
  )
}
