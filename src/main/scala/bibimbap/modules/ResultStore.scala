package bibimbap
package modules

import akka.actor._
import bibtex._

class ResultStore(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "results"

  private var results = List[SearchResult]()

  override def receive: Receive = {
    case Command1("list") | Command1("show") | Command1("last") =>
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
      case _ =>
        None
    }
  }

  private def doShow(res: SearchResult) {
    console ! Out(res.entry.toString)
  }

  private def displayResults() {
    var displayLegend = Map(
      "alternatives" -> false,
      "managed"      -> false,
      "managedMod"   -> false,
      "managedInv"   -> false,
      "incomplete"   -> false
    )

    var i = 0
    for (res <- results) {
      val spc = if ((i < 10) && (results.size > 10)) " " else ""

      val sources = if (res.alternatives.isEmpty) {
        " "
      } else {
        displayLegend += "alternatives" -> true
        "\u2026"
      }

      val symbol = if (res.isManaged) {
        if (!res.entry.isValid) {
          displayLegend += "managedInv" -> true
        } else if(res.isEdited) {
          displayLegend += "managedMod" -> true
        } else {
          displayLegend += "managed"    -> true
        }

        "\u2714"
      } else if (res.entry.isValid) {
        " "
      } else {
        displayLegend += "incomplete"   -> true
        "\u2049"
      }

      val color = if (res.entry.isValid) {
        if (res.isEdited) {
          Console.YELLOW
        } else {
          Console.GREEN
        }
      } else {
        Console.RED
      }

      val status = if (settings.colors) {
        color+Console.BOLD+symbol+Console.RESET
      } else {
        symbol
      }

      console ! Out(" "+sources+status+" "+spc+"["+i+"] "+res.entry.inlineString)
      i += 1
    }

    if (displayLegend.exists(_._2 == true)) {
      console ! Out("")
      console ! Out(" Legend:")
      if (displayLegend("alternatives")) console ! Out("   "+Console.BOLD+"\u2026"+Console.RESET+" : Alternatives available")
      if (displayLegend("managed"))      console ! Out("   "+Console.BOLD+Console.GREEN +"\u2714"+Console.RESET+" : Managed")
      if (displayLegend("managedMod"))   console ! Out("   "+Console.BOLD+Console.YELLOW+"\u2714"+Console.RESET+" : Managed (edited)")
      if (displayLegend("managedInv"))   console ! Out("   "+Console.BOLD+Console.YELLOW+"\u2714"+Console.RESET+" : Managed (incomplete)")
      if (displayLegend("incomplete"))   console ! Out("   "+Console.BOLD+Console.RED +"\u2049"+Console.RESET  +" : Incomplete")
    }
    if (results.isEmpty) {
      console ! Info("No match")
    }
  }

  val helpItems = Map(
    "list"   -> HelpEntry("list",             "Displays the current list of results."),
    "last"   -> HelpEntry("last",             "Displays the current list of results."),
    "show"   -> HelpEntry("show <result>",    "Displays the bib entry for the <results>th search result.")
  )
}
