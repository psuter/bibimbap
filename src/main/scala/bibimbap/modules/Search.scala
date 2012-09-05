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

  private val Range  = """(\d+)-(\d+)""".r
  private val Single = """(\d+)""".r
  private def getSources(index: String): Option[List[SearchSource]] = {
    index match {
      case "*" =>
        Some(searchSources)
      case Range(lower, upper) =>
        val l = lower.toInt
        val u = upper.toInt
        if (l <= u && l >= 0 && u < searchSources.size) {
          Some(searchSources.slice(l, u + 1))
        } else {
          None
        }
      case Single(index) =>
        val i = index.toInt
        if (i < searchSources.size && i >= 0) {
          Some(List(searchSources(i)))
        } else {
          None
        }
      case _ =>
        None
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

    case Command3("sources", "enable", ind) =>
      getSources(ind) match {
        case Some(rs) =>
          for (r <- rs) {
            r.isActive = true
          }
          console ! Success("Source(s) activated")
        case None =>
          console ! Error("Invalid source")
      }
      sender ! CommandSuccess

    case Command3("sources", "disable", ind) =>
      getSources(ind) match {
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
      val actor = context.actorOf(Props(new SearchBibtex(self, console, settings, path)))

      syncCommand(actor, Start)

      searchSources = searchSources :+ SearchSource(actor, "bibtex: "+path)

      console ! Success("Source added!")
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

    case SearchOne(terms) =>
      val results = doSearch(terms)
      sender ! SearchResults(results.headOption.toList)

    case x =>
      super.receive(x)
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
      SearchResult(res.head.entry,
                   res.flatMap(_.sources).toSet,
                   res.map(_.relevance).max,
                   isEdited     = res.exists(_.isEdited),
                   isManaged    = res.exists(_.isManaged),
                   oldEntry     = res.map(_.oldEntry).reduceLeft(_ orElse _),
                   alternatives = res.tail.map(_.entry).toSet.filter(_ != res.head.entry)
                 )
    }

    val sorted = combined.toList.sortBy(- _.relevance)

    sorted
  }

  private lazy val fileNamesCompletor = new jline.FileNameCompletor()

  override def complete(buffer: String, pos: Int): (List[String], Int) = {
    import collection.JavaConversions._
    if (buffer.startsWith("sources add ") && pos >= "sources add ".length) {
      val newbuffer = buffer.substring("sources add ".length, buffer.length) 
      val newpos    = pos - "sources add ".length

      val list  = new java.util.ArrayList[String]()
      val index = fileNamesCompletor.complete(newbuffer, newpos, list)

      (list.toList, index + "sources add ".length)
    } else {
      (Nil, 0)
    }
  }

  val helpItems = Map(
    "search"          -> HelpEntry("search <terms..>",         "Searches for <terms> using the various search providers"),
    "sources"         -> HelpEntry("sources",                  "Lists available search sources"),
    "sources add"     -> HelpEntry("sources add <path>",       "Adds bibfile as additional source"),
    "sources enable"  -> HelpEntry("sources enable <index>",   "Enables source"),
    "sources disable" -> HelpEntry("sources disable <index>",  "Disables source")
  )
}
