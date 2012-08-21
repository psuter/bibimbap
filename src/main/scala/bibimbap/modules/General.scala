package bibimbap
package modules

import akka.actor._

class General(val repl: ActorRef, val logger: ActorRef) extends Module {
  val name = "General"

  def receive = {
    case Command(line) =>
      line match {
        case Command1("exit") | Command1("quit")=>
          repl ! Shutdown
          sender ! CommandSuccess
        case _ =>
          sender ! CommandUnknown
      }
  }
}
