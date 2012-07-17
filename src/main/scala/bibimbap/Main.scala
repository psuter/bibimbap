package bibimbap

import bibimbap.data._

import jline._

import scala.collection.mutable.{Map=>MutableMap}
import scala.sys.process._

import java.io.{File,FileWriter}

object Main {
  private val homeDir = System.getProperty("user.home") + System.getProperty("file.separator")

  private val configFileName = homeDir + ".bibimbapconfig"
  private val historyFileName = homeDir + ".bibimbaphistory"

  private val replID = "bibimbap> "


  def main(args : Array[String]) : Unit = {
    sayHello
    val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

    val theMainModule = mainModule(settings)
    
    val reader = new ConsoleReader

    val testCompletor = new SimpleCompletor(theMainModule.allSubKeywords.toArray)
    reader.addCompletor(testCompletor)

    val history = new History(new File(historyFileName))
    reader.setHistory(history)

    var line : String = null
    while(true) {
      line = reader.readLine(replID)
      if(line == null) {
        sys.exit(0)
      }
      line = line.trim
      if(line != "") {
        theMainModule.executeLine(line)
      }
      settings.logger.info("")
    }
  }


  private def mainModule(settings : Settings) = new Module(settings) {
    val name = ""
    val keyword = "<main>"

    private var searchHistory : MutableMap[Int,SearchResultEntry] = MutableMap.empty

    import settings.logger.{info,warn}

    // Common superclass for all actions that do something with a particular
    // search result.
    sealed abstract class SearchResultAction(kw : String) extends Action[Unit](kw) {
      def run(sre : SearchResultEntry) : Unit

      def run(args : String*) : Unit = {
        if(args.isEmpty) {
          warn("Please provide a search result id.")
        } else {
          argAsInt(args(0)) match {
            case None => warn("Please provide a numerical search result id.")
            case Some(i) => searchHistory.get(i) match {
              case None => warn("No entry [" + i + "] in search history.")
              case Some(sre) => run(sre)
            }
          }
        }
      }
    }

    override val moreActions = List(
      new Action[Nothing]("exit") {
        val description = "Exit the program."
        def run(args : String*) : Nothing = {
          sys.exit(0)
        }
      },

      new Action[Unit]("search") {
        val description = "Search for bibliographical entries."
        def run(args : String*) : Unit = {
          searchHistory.clear

          var i = 0
          for(sm <- subModules collect { case srm : SearchModule => srm }) {
            var first = true
            for(searchResultEntry <- sm.searchAction.run(args : _*).take(10)) {
              if(first) {
                info("[" + sm.dataSourceName + "]")
                first = false
              }
              val SearchResultEntry(entry, _, _, src) = searchResultEntry
              val spc = if(i < 10) " " else ""
              info(spc + "[" + i + "] " + entry.inlineString)
              searchHistory(i) = searchResultEntry
              i += 1
            }
          }
        }
      },

      new SearchResultAction("show") {
        val description = "Examine entry resulting of a search."

        def run(sre : SearchResultEntry) {
          println(sre.entry)
        }
      },

      new SearchResultAction("open") {
        private val devNullLogger = new ProcessLogger {
          def out(s : =>String) = {}
          def err(s : =>String) = {}
          def buffer[T](f : =>T) = f
        }
        val description = "If available, open link for entry resulting of a search."
        def run(sre : SearchResultEntry) {
          sre.link match {
            case None => warn("No available link for entry.")
            case Some(lnk) => settings.get("general", "url.open") match {
              case None => {
                warn("Setting 'url.open' in category 'general' is not set.")
                warn("Don't know how to open " + lnk)
              }
              case Some(runner) => {
                info("Opening " + lnk + " ...")
                (runner + " " + lnk).run(devNullLogger)
              }
            }
          }
        }
      },

      new SearchResultAction("import") {
        val description = "Imports a search result into the managed BibTeX file."

        def run(sre : SearchResultEntry) {
          val fn = settings("general", "bib.filename")
          val fw = new FileWriter(new File(fn), true)
          fw.write(sre.entry.toString)
          fw.write("\n\n")
          fw.close

          import java.awt.Toolkit
          import java.awt.datatransfer.StringSelection
          val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
          val stringSel = new StringSelection(sre.entry.getKey)
          clipboard.setContents(stringSel, stringSel)

          settings.logger.info("Imported: \\cite{"+sre.entry.getKey+"}")

          for(sub <- subModules) {
            sub.onImport(sre)
          }
        }
      }
    )

    override val subModules = List(
      new cache.CacheModule(settings),
      new dblp.DBLPModule(settings)
    )
  }

  private def sayHello {
    println("""         __    _ __    _           __                     """)
    println("""   ———  / /_  (_) /_  (_)___ ___  / /_  ____ _____  ——————""")
    println("""  ———  / __ \/ / __ \/ / __ `__ \/ __ \/ __ `/ __ \  ———— """)
    println(""" ———  / /_/ / / /_/ / / / / / / / /_/ / /_/ / /_/ /  ———  """)
    println("""———  /_.___/_/_.___/_/_/ /_/ /_/_.___/\__,_/ .___/  ———   """)
    println("""                                          /_/             """)
    println("")
  }
}

