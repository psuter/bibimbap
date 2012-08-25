package bibimbap

import akka.actor._
import akka.util.Timeout
import scala.concurrent.util.duration._

trait Module extends Actor {
  val settings: Settings
  val logger: ActorRef
  val repl: ActorRef

  val name: String

  def handleCommand: Receive;

  def receive: Receive = {
    case Command(Command2("help", command)) =>
      if (helpItems contains command) {
        helpItems(command).display(logger)
      }
      sender ! CommandSuccess
    case Command(Command1("help")) =>
      for ((command, hi) <- helpItems) {
        helpItems(command).displayShort(logger)
      }
      sender ! CommandSuccess

    case Command(cmd) =>
      if (handleCommand isDefinedAt cmd) {
        handleCommand(cmd)
        sender ! CommandSuccess
      } else {
        sender ! CommandUnknown
      }
  }

  implicit val timeout = Timeout(15.seconds)

  val helpItems: Map[String, HelpEntry];
}
