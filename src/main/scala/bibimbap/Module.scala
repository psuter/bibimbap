package bibimbap

import akka.actor._
import akka.util.Timeout
import scala.concurrent.util.duration._

trait Module extends Actor {
  val settings: Settings
  val logger: ActorRef
  val repl: ActorRef

  val name: String

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

    case _ =>
      sender ! CommandUnknown
  }

  implicit val timeout = Timeout(15.seconds)

  val helpItems: Map[String, HelpEntry];
}
