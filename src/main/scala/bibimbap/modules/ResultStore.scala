package bibimbap
package modules

import akka.actor._
import bibtex._

class ResultStore(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "results"

  private var results = List[SearchResult]()

  override def receive: Receive = {
    case Command1("list") | Command1("show") =>
      displayResults()
      sender ! CommandSuccess
    case Command2("show", ind) =>
      getResults(ind) match {
        case Some(rs) =>
          for (r <- rs) {
            doShow(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandSuccess

    case SearchResults(newResults) =>
      results = newResults
      displayResults()

      sender ! CommandSuccess

    case ReplaceResults(ind, newResults) =>
      getResults(ind) match {
        case Some(rs) =>
          results = results.map((rs zip newResults).toMap.orElse{ case x => x })
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandSuccess

    case GetResults(index) =>
      sender ! SearchResults(getResults(index).getOrElse(Nil))

    case ShowResults =>
      displayResults()
      sender ! CommandSuccess

    case x =>
      super.receive(x)
  }

  private val Range  = """(\d+)-(\d+)""".r
  private val Single = """(\d+)""".r
  private def getResults(index: String): Option[List[SearchResult]] = {
    index match {
      case "*" =>
        Some(results)
      case Range(lower, upper) =>
        val l = lower.toInt
        val u = upper.toInt
        if (l <= u && l >= 0 && u < results.size) {
          Some(results.slice(l, u + 1))
        } else {
          None
        }
      case Single(index) =>
        val i = index.toInt
        if (i < results.size && i >= 0) {
          Some(List(results(i)))
        } else {
          None
        }
    }
  }

  private def doShow(res: SearchResult) {
    console ! Out(res.entry.toString)
  }

  private def displayResults() {
    var i = 0
    for (res <- results) {
      val spc = if (i < 10) " " else ""

      val symbol = if (res.sources.contains("managed")) {
        "m"
      } else if (res.entry.isValid) {
        " "
      } else {
        "!"
      }

      val color = if (res.entry.isValid) Console.GREEN else Console.RED

      val status = color+Console.BOLD+symbol+Console.RESET

      console ! Info("["+i+spc+" "+status+"] "+res.entry.inlineString)
      i += 1
    }
    if (results.isEmpty) {
      console ! Info("No match")
    }
  }

  val helpItems = Map(
    "list"   -> HelpEntry("list",             "Displays the current list of results."),
    "show"   -> HelpEntry("show <result>",    "Displays the bib entry for the <results>th search result.")
  )
}
