package bibimbap
package modules

import akka.actor._
import java.util.concurrent.TimeoutException

import bibtex._

case class SearchSource(actor: ActorRef, name: String, var isActive: Boolean = true)

class Search(val repl: ActorRef,
             val console: ActorRef,
             val settings: Settings,
             var searchSources: List[SearchSource]) extends Module {

  val name = "search"

  override val dependsOn = Set("results")

  lazy val resultsModule = modules("results")

  private def addSource(path: String) {
    val actor = context.actorOf(Props(new SearchBibtex(self, console, settings, path)))

    syncCommand(actor, Start) match {
      case Some(CommandSuccess) =>
        searchSources = searchSources :+ SearchSource(actor, "bibtex: "+path)
        console ! Success("Source added!")
      case Some(CommandError(err)) =>
        console ! Error("Error adding source: "+err)
      case Some(CommandException(e)) =>
        console ! Error("Error adding source: "+e.getMessage)
      case _ =>
    }

  }

  override def receive: Receive = {
    case Command1("sources") =>
      for ((s, i) <- searchSources.zipWithIndex) {
        val spc = if ((i < 10) && (searchSources.size > 10)) " " else ""

        val status = if(s.isActive) {
          settings.GREEN+"on "+settings.RESET 
        } else {
          settings.RED+"off"+settings.RESET 
        }

        console ! Out(" "+status+" "+spc+"["+i+"] "+s.name)
      }
      sender ! CommandSuccess

    case Command3("sources", "enable", Indices(ids)) =>
      ids.within(searchSources) match {
        case Some(rs) =>
          for (r <- rs) {
            r.isActive = true
          }
          console ! Success("Source(s) activated")
        case None =>
          console ! Error("Invalid source")
      }
      sender ! CommandSuccess

    case Command3("sources", "disable", Indices(ids)) =>
      ids.within(searchSources) match {
        case Some(rs) =>
          for (r <- rs) {
            r.isActive = false
          }
          console ! Success("Source(s) disabled")
        case None =>
          console ! Error("Invalid source")
      }
      sender ! CommandSuccess

    case Command3("sources", "add", path) =>
      addSource(path)
      sender ! CommandSuccess

    case Command3("load", path) =>
      addSource(path)
      sender ! CommandSuccess

    case CommandL("search", args) =>
      val results = doSearch(args)

      syncCommand(resultsModule, SearchResults(results))
      syncCommand(resultsModule, ShowResults(args))

      sender ! CommandSuccess

    case ImportedResult(res) =>
      for (m <- searchSources.map(_.actor)) {
        m ! ImportedResult(res)
      }

    case Search(terms) =>
      val results = doSearch(terms)
      sender ! SearchResults(results)

    case SearchSimilar(entry: BibTeXEntry) =>
      if (preciseEnough(entry)) {
        val results = doSearch(termsFromEntry(entry))
        sender ! SimilarEntry(entry, results.headOption.map(_.entry))
      } else {
        sender ! SimilarEntry(entry, None)
      }

    case x =>
      super.receive(x)
  }

  private def preciseEnough(e: BibTeXEntry): Boolean = {
    !e.title.isEmpty
  }

  private def termsFromEntry(e: BibTeXEntry): List[String] = {
    (e.title ++ e.authors).map(_.toJava).toList
  }

  def activeSources: List[SearchSource] = searchSources.filter(_.isActive)

  private def doSearch(args: List[String]): List[SearchResult] = {
    try {
      val resultsPerSearch = dispatchMessage[SearchResults](Search(args), activeSources.map(_.actor))
      combineResults(resultsPerSearch)
    } catch {
      case e: TimeoutException =>
        console ! Error("Failed to gather search results in time")
        Nil
    }
  }

  private def combineResults(resultss: List[SearchResults]): List[SearchResult]= {
    import scala.collection.mutable.ListBuffer

    var equivClasses = ListBuffer[ListBuffer[SearchResult]]()

    for (res <- resultss.flatMap(_.entries)) {
      equivClasses.find(_.head.entry like res.entry) match {
        case Some(cl) => cl.append(res)
        case None     => equivClasses.append(new ListBuffer[SearchResult]() :+ res)
      }
    }

    val combined = for (res <- equivClasses) yield {
      var mainEntry = res.head.entry
      var fields = mainEntry.entryMap

      for (entry <- res.tail.map(_.entry); (k, v) <- entry.entryMap if !fields.contains(k)) {
        fields += k -> v
      }

      BibTeXEntry.fromEntryMap(res.map(_.entry.tpe).reduceLeft(_ orElse _), mainEntry.key, fields, console ! Error(_)) match {
        case Some(newEntry) =>
          Some(SearchResult(newEntry,
                       res.flatMap(_.sources).toSet,
                       res.map(_.relevance).max,
                       isEdited     = res.exists(_.isEdited),
                       isManaged    = res.exists(_.isManaged),
                       oldEntry     = res.map(_.oldEntry).reduceLeft(_ orElse _),
                       alternatives = res.map(_.entry).toSet
                     ))
        case _ =>
          None
      }
    }

    val sorted = combined.flatten.toList.sortBy(- _.relevance)

    sorted
  }

  override def complete(buffer: String, pos: Int): (List[String], Int) = {
    val SourcesAdd = FileCompletor("sources add ")

    (buffer, pos) match {
      case SourcesAdd(alts, pos) => (alts, pos)
      case _ => (Nil, 0)
    }
  }

  val helpItems = Map(
    "search"          -> HelpEntry("search <terms..>",         "Searches for <terms> using the various search providers"),
    "sources"         -> HelpEntry("sources",                  "Lists available search sources"),
    "sources add"     -> HelpEntry("sources add <path>",       "Adds bibfile as additional source"),
    "load"            -> HelpEntry("load <path>",              "Adds bibfile as additional source"),
    "sources enable"  -> HelpEntry("sources enable <index>",   "Enables source"),
    "sources disable" -> HelpEntry("sources disable <index>",  "Disables source")
  )
}
