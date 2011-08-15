package bibimbap

import bibimbap.data._

import jline._

import scala.collection.mutable.{Map=>MutableMap}

object Main {
  private val configFileName =
    System.getProperty("user.home") +
    System.getProperty("file.separator") +
    ".bibimbapconfig"

  private val replID = "bibimbap> "

  def main(args : Array[String]) : Unit = {
    val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

    val theMainModule = mainModule(settings)

    val testCompletor = new SimpleCompletor(theMainModule.allSubKeywords.toArray)
    
    var line : String = null
    val reader = new ConsoleReader
    reader.addCompletor(testCompletor)

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

    override val moreActions = List(
      new Action[Unit] {
        val keyword = "info"
        val description = "Provides information."

        def run(args : String*) {
          info("I'm providing information ! Also, ignoring your arguments : " + args.mkString(", "))
        }
      },

      new Action[Nothing] {
        val keyword = "exit"
        val description = "Exits the program."
        def run(args : String*) : Nothing = {
          sys.exit(0)
        }
      },

      new Action[Unit] {
        val keyword = "search"
        val description = "Searches for bibliographical entries."
        def run(args : String*) : Unit = {
          searchHistory.clear

          for(sm <- subModules; sa <- sm.searchAction) {
            for((searchResultEntry, i) <- sa.run(args : _*).take(10).zipWithIndex) {
              val (entry, _) = searchResultEntry
              info("[" + i + "] " + entry.inlineString)
              searchHistory(i) = searchResultEntry
            }
          }
        }
      },

      new Action[Unit] {
        val keyword = "show"
        val description = "Examine entry resulting of a search."
        def run(args : String*) : Unit = {
          if(args.isEmpty) {
            warn("Please provide a search result id.")
          } else {
            argAsInt(args(0)) match {
              case None => warn("Please provide a numerical search result id.")
              case Some(i) => searchHistory.get(i) match {
                case None => warn("No entry [" + i + "] in search history.")
                case Some(sre) => println(sre._1)
              }
            }
          }
        }
      }
    )

    override val subModules = List(
      new dblp.DBLPModule(settings),
      new DummyModule(settings)
    )

    
  }
}

class DummyModule(settings : Settings) extends Module(settings) {
  val name = "Dummy module."
  val keyword = "dummy"

  override val moreActions = List(new Action[Unit] { val keyword = "hi"; val description = "Say hi."; def run(args : String*) { settings.logger.info("Hiiiii !") } })
}
