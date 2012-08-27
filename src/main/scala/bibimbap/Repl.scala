package bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.util.duration._
import scala.concurrent.ExecutionContext

import jline._

import java.io.File

class Repl(homeDir: String, configFileName: String, historyFileName: String) extends Actor with ActorHelpers {
  val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

  val handle = "bibimbap> "
  val reader = new ConsoleReader

  val logger = context.actorOf(Props(new Logger(settings)), name = "Logger")

  var modules = Map[String, ActorRef]()

  override def preStart = {
    sayHello()

    val history = new History(new File(historyFileName))
    reader.setHistory(history)

    startModules()
  }

  def startModules() {
    import bibimbap.modules._

    modules = Map(
      "general" -> context.actorOf(Props(new General(self, logger, settings)),      name = "general"),
      "search"  -> context.actorOf(Props(new Search(self, logger, settings)),       name = "search"),
      "results" -> context.actorOf(Props(new ResultStore(self, logger, settings)),  name = "results")
    )

  }

  def dispatchCommand(cmd: Command) {
    implicit val timeout = Timeout(365.day)

    val results = dispatchCommand[CommandResult](cmd, modules.values.toList) 

    val isKnown = results exists {
      case CommandSuccess  =>
        true
      case CommandException(e)    =>
        logger ! Error(e.getMessage)
        false
      case CommandError(msg)      =>
        logger ! Error(msg)
        false
      case _ =>
        false
    }

    if (!isKnown && !results.isEmpty) {
      cmd match {
        case InputCommand(line) =>
          logger ! Error("Unknown command: "+line)
        case _ =>
          logger ! Error("Unknown command type: "+cmd)
      }
    }
  }

  def receive = {
    case Start =>
      dispatchCommand(OnStartup(modules = modules))
      self ! ReadLine

    case ReadLine =>
      // Flush message box of Logger and tore subsequent messages to avoid
      // messing with input
      Await.result(logger ? LoggerFlush, 1.seconds)

      println()
      var line = reader.readLine(handle)
      logger ! LoggerContinue

      if(line == null) {
        sys.exit(0)
      } else {
        line = line.trim
        if(line != "") {
          dispatchCommand(InputCommand(line))
        }
      }

      self ! ReadLine
    case Shutdown =>
      dispatchCommand(OnShutdown())
      logger ! Out("Bye.")
      sys.exit(0)
  }

  private def sayHello() {
    logger ! Out("""         __    _ __    _           __                        """)
    logger ! Out("""   ———  / /_  (_) /_  (_)___ ___  / /_  ____ _____  ——————   """)
    logger ! Out("""  ———  / __ \/ / __ \/ / __ `__ \/ __ \/ __ `/ __ \  ————    """)
    logger ! Out(""" ———  / /_/ / / /_/ / / / / / / / /_/ / /_/ / /_/ /  ———     """)
    logger ! Out("""———  /_.___/_/_.___/_/_/ /_/ /_/_.___/\__,_/ .___/  ———      """)
    logger ! Out("""                                          /_/         비빔밥 """)
    logger ! Out("")
  }
}
