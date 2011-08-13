package bibimbap

import jline._

object Main {
  private lazy val testCompletor = new SimpleCompletor(mainModule.allSubKeywords.toArray)

  private val replID = "bibimbap> "
  def main(args : Array[String]) : Unit = {
    
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
        mainModule.executeLine(line)
      }
      println("")
    }
  }

  private val mainModule = new Module(DefaultSettings) {
    val name = ""
    val keyword = "<main>"

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
      }
    )

    override val subModules = List(
      new dblp.DBLPModule(settings),
      DummyModule
    )
  }
}

object DummyModule extends Module(DefaultSettings) {
  val name = "Dummy module."
  val keyword = "dummy"

  override val moreActions = List(new Action[Unit] { val keyword = "hi"; val description = "Say hi."; def run(args : String*) { DefaultSettings.logger.info("Hiiiii !") } })
}
