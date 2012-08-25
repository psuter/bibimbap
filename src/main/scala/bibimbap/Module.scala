package bibimbap

import akka.actor._
import akka.util.Timeout
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.util.duration._
import scala.concurrent.util.Duration

trait Module extends Actor {
  val settings: Settings
  val logger: ActorRef
  val repl: ActorRef

  val name: String

  val dependsOn = Set[String]()
  var modules   = Map[String, ActorRef]()

  def receive: Receive = {
    case Command2("help", command) =>
      if (helpItems contains command) {
        helpItems(command).display(logger)
      }
      sender ! CommandSuccess
    case Command1("help") =>
      for ((command, hi) <- helpItems) {
        helpItems(command).displayShort(logger)
      }
      sender ! CommandSuccess

    case os: OnStartup =>
      startup(os)
      sender ! CommandSuccess

    case os: OnShutdown =>
      shutdown(os)
      sender ! CommandSuccess

    case _ =>
      sender ! CommandUnknown
  }

  def startup(os: OnStartup) {
    modules = os.modules;

    val depsFound = modules.keySet & dependsOn

    if (depsFound.size < dependsOn.size) {
      for (dep <- dependsOn if !(modules contains dep)) {
        logger ! Error("Missing dependency: module '"+name+"' requires module '"+dep+"'")
      }
      repl ! Shutdown
    }
  }

  implicit val timeout = Timeout(15.seconds)

  def syncSend(actor: ActorRef, cmd: Any) = {
    Await.result((actor ? cmd), timeout.duration)
  }

  def shutdown(os: OnShutdown) {

  }


  val helpItems: Map[String, HelpEntry];
}
