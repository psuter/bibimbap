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

class Repl(homeDir: String, configFileName: String, historyFileName: String) extends Actor with ActorHelpers {
  val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

  val console = context.actorOf(Props(new Console(self, settings, historyFileName)), name = "Console")

  var modules = Map[String, ActorRef]()

  override def preStart = {
    sayHello()

    startModules()
  }

  def startModules() {
    import bibimbap.modules._

    val managed      = context.actorOf(Props(new Managed(self, console, settings)),      name = "managed");
    val searchBibtex = context.actorOf(Props(new SearchBibtex(self, console, settings)), name = "searchBibtex");

    val searchProviders = List(
      managed,
      context.actorOf(Props(new SearchLocal(self, console, settings)), name = "searchLocal"),
      context.actorOf(Props(new SearchDBLP(self, console, settings)), name = "searchDBLP"),
      searchBibtex
    )

    modules = Map(
      "general" -> context.actorOf(Props(new General(self, console, settings)),                 name = "general"),
      "search"  -> context.actorOf(Props(new Search(self, console, settings, searchProviders)), name = "search"),
      "results" -> context.actorOf(Props(new ResultStore(self, console, settings)),             name = "results"),
      "wizard"  -> context.actorOf(Props(new Wizard(self, console, settings)),                  name = "wizard"),
      "managed" -> managed,
      "bibtex"  -> searchBibtex
    )

  }

  def dispatchCommand(cmd: Command) {
    implicit val timeout = Timeout(365.day)

    val results = dispatchMessage[CommandResult](cmd, modules.values.toList) 

    val isKnown = results exists {
      case CommandSuccess  =>
        true
      case CommandException(e)    =>
        console ! Error(e.getMessage)
        false
      case CommandError(msg)      =>
        console ! Error(msg)
        false
      case _ =>
        false
    }

    if (!isKnown && !results.isEmpty) {
      cmd match {
        case InputCommand(line) =>
          console ! Error("Unknown command: "+line)
        case _ =>
          console ! Error("Unknown command type: "+cmd)
      }
    }
  }

  def receive = {
    case Start =>
      dispatchCommand(OnStartup(modules = modules))
      syncCommand(console, OnStartup(modules = modules))
      self ! ReadLine()

    case ReadLine(_, _) =>
      implicit val timeout = neverTimeout

      syncMessage[LineRead](console, ReadLine()) match {
        case Some(EOF) =>
          // TODO: find a better way to exit
          sys.exit(0)
        case Some(LineRead(line)) =>
          val cmd = line.trim
          if(cmd != "") {
            dispatchCommand(InputCommand(cmd))
          }
        case _ =>
      }

      self ! ReadLine()
    case Shutdown =>
      dispatchCommand(OnShutdown())
      console ! Out("Bye.")
      // TODO: find a better way to exit
      sys.exit(0)
  }

  private def sayHello() {
    console ! Out("""         __    _ __    _           __                        """)
    console ! Out("""   ———  / /_  (_) /_  (_)___ ___  / /_  ____ _____  ——————   """)
    console ! Out("""  ———  / __ \/ / __ \/ / __ `__ \/ __ \/ __ `/ __ \  ————    """)
    console ! Out(""" ———  / /_/ / / /_/ / / / / / / / /_/ / /_/ / /_/ /  ———     """)
    console ! Out("""———  /_.___/_/_.___/_/_/ /_/ /_/_.___/\__,_/ .___/  ———      """)
    console ! Out("""                                          /_/         비빔밥 """)
    console ! Out("")
  }
}
