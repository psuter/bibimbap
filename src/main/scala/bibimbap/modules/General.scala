package bibimbap
package modules

import akka.actor._

class General(val repl: ActorRef, val logger: ActorRef, val settings: Settings) extends Module {
  val name = "General"

  def receive = {
    case Command(Command1("exit")) | Command(Command1("quit")) =>
      repl ! Shutdown
      sender ! CommandSuccess
    case _ =>
      sender ! CommandUnknown
  }
}
