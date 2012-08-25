package bibimbap

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

class Repl(homeDir: String, configFileName: String, historyFileName: String)  extends Actor {
  implicit val timeout = Timeout(20.seconds)

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
    val futures = modules.values.map(actor => (actor ? cmd).mapTo[CommandResult] recover {
      case e: Throwable => CommandException(e)
    }).toList

    try {
      val responses = Await.result(Future.sequence(futures), timeout.duration) collect {
        case CommandSuccess      => ()
        case CommandException(e) => logger ! Error(e.getMessage)
        case CommandError(msg)   => logger ! Error(msg)
      }

      if (responses.isEmpty) {
        logger ! Error("Unknown command: "+cmd)
      }
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        logger ! Error("Timeout while waiting for command result")
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
        println("Unsupported Terminal")
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
