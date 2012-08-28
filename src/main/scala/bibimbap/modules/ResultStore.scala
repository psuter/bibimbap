package bibimbap
package modules

import akka.actor._
import data._

class ResultStore(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "results"

  private var results = List[SearchResult]()

  override def receive: Receive = {
    case Command1("list") | Command1("show") =>
      displayResults()
      sender ! CommandSuccess
    case Command2("import", ind) =>
      getResults(ind) match {
        case Some(rs) =>
          for (r <- rs) {
            doImport(r)
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

  private def doImport(res: SearchResult) {
    import java.io.{FileWriter,File}

    val fn = settings("general", "bib.filename")
    val fw = new FileWriter(new File(fn), true)
    fw.write(res.entry.toString)
    fw.write("\n\n")
    fw.close

    import java.awt.Toolkit
    import java.awt.datatransfer.StringSelection
    try {
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val stringSel = new StringSelection(res.entry.getKey)
      clipboard.setContents(stringSel, stringSel)
    } catch {
      case e: java.awt.HeadlessException =>
        console ! Warning("Could not store in clipboard: "+e.getMessage.trim)
    }

    // Inform search module that we imported this
    modules("search") ! ImportedResult(res)

    console ! Success("Imported: \\cite{"+res.entry.getKey+"}")
  }

  private def doShow(res: SearchResult) {
    console ! Out(res.entry.toString)
  }

  private def displayResults() {
    var i = 0
    for (res <- results) {
      val spc = if (i < 10) "" else " "

      val colSourceCache     = if (res.sources.contains("cache")) Console.YELLOW+"c"+Console.RESET else " "
      val colSourceDBLP      = if (res.sources.contains("dblp"))  Console.YELLOW+"w"+Console.RESET else " "
      val colSourceImported  = if (res.sources.contains("managed"))  Console.GREEN+"I"+Console.RESET else " "
      val colSourceLoad      = if (res.sources.contains("loaded"))   Console.GREEN+"L"+Console.RESET else " "
      val colInvalid         = if (!res.entry.isValid) Console.RED+"!"+Console.RESET else " "

      val extraCols = colSourceCache+colSourceDBLP+colSourceImported+colSourceLoad+colInvalid

      console ! Info("["+i+spc+" "+extraCols+"] "+res.entry.inlineString)
      i += 1
    }
    if (results.isEmpty) {
      console ! Info("No match")
    }
  }

  val helpItems = Map(
    "list"   -> HelpEntry("list",             "Displays the current list of results."),
    "import" -> HelpEntry("import <result>",  "Imports the <result>th item from the last search results into managed.bib"),
    "show"   -> HelpEntry("show <result>",    "Displays the bib entry for the <results>th search result.")
  )
}
