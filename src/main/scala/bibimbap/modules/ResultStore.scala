package bibimbap
package modules

import akka.actor._
import bibtex._

class ResultStore(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "results"

  private var results = List[SearchResult]()

  override def receive: Receive = {
    case Command1("list") | Command1("show") | Command1("last") =>
      displayResults(Nil)
      sender ! CommandSuccess

    case Command2("bib", ind) =>
      getResults(ind) match {
        case Some(rs) =>
          for (r <- rs) {
            doBib(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
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

    case ShowResults(terms) =>
      displayResults(terms)
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
    def inBold(str: String): String    = if (settings.colors) Console.BOLD+str+Console.RESET else str
    def inRedBold(str: String): String = if (settings.colors) Console.BOLD+Console.RED+str+Console.RESET else str
    res.entry.display(console ! Out(_), inBold, inRedBold)
  }

  private def doBib(res: SearchResult) {
    console ! Out(res.entry.toString)
  }

  case class ResultFlag(has: SearchResult => Boolean, symbol: String, legend: String)

  val flagsColumns = List(
    List(
      ResultFlag({res => res.isManaged && res.entry.isValid  }, if (settings.colors) Console.GREEN+"\u2714"+Console.RESET else "\u2714",   "Managed"),
      ResultFlag({res => res.isManaged && !res.entry.isValid }, if (settings.colors) Console.RED+"\u2714"+Console.RESET else "\u2714",     "Managed (incomplete)"),
      ResultFlag({res => !res.entry.isValid },                  if (settings.colors) Console.RED+"\u2049"+Console.RESET else "\u2049",     "Incomplete")
    ),
    List(
      ResultFlag(_.isEdited, if (settings.colors) Console.YELLOW+Console.BOLD+"e"+Console.RESET else "e", "Edited")
    ),
    List(
      ResultFlag(!_.alternatives.isEmpty, if (settings.colors) Console.BLUE+Console.BOLD+"+"+Console.RESET else "+", "Multiple Alternatives")
    )
  )

  private def displayResults(terms: List[String]) {
    def highlight(str: String): String = {
      if (settings.colors && !terms.isEmpty) {
        import java.util.regex.Pattern
        str.replaceAll("(?i)"+terms.map(Pattern.quote).mkString("(", "|", ")"), Console.BOLD+Console.RED+"$0"+Console.RESET)
      } else {
        str
      }
    }

    var columnsUsed = Set[Int]()
    var flagsUsed   = Set[ResultFlag]()

    // Pre-checks what columns will be displayed
    for (res <- results; (flags, column) <- flagsColumns.zipWithIndex; flag <- flags if flag.has(res)) {
      columnsUsed += column
      flagsUsed += flag
    }

    for ((res, i) <- results.zipWithIndex) {
      val spc = if ((i < 10) && (results.size > 10)) " " else ""

      val columns = for (col <- columnsUsed.toSeq.sorted) yield {
        flagsColumns(col).find(_.has(res)) match {
          case Some(flag) =>
            flag.symbol
          case None =>
            " "
        }
      }

      console ! Out(" "+columns.mkString+" "+spc+"["+i+"] "+highlight(res.entry.inlineString))
    }

    if (!flagsUsed.isEmpty) {
      console ! Out("")
      console ! Out(" Legend:")
      for (flag <- flagsUsed) {
        console ! Out("   "+flag.symbol+" : "+flag.legend)
      }
    }
    if (results.isEmpty) {
      console ! Info("No match")
    }
  }

  val helpItems = Map(
    "list"   -> HelpEntry("list",             "Displays the current list of results."),
    "last"   -> HelpEntry("last",             "Displays the current list of results."),
    "bib"    -> HelpEntry("bib  <result>",    "Displays the bib entry for the <results>th search result."),
    "show"   -> HelpEntry("show <result>",    "Displays the entry for the <results>th search result.")
  )
}
