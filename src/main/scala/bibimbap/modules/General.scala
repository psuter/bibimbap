package bibimbap
package modules

import akka.actor._

class General(val repl: ActorRef, val logger: ActorRef, val settings: Settings) extends Module {
  val name = "General"

  override def receive: Receive = {
    case Command1("exit") | Command1("quit") =>
      repl ! Shutdown
    case _ =>
      super.receive
  }

  val helpItems = Map(
    "exit" -> HelpEntry("exit", "Exits bibimbap"),
    "quit" -> HelpEntry("quit", "Exits bibimbap")
  )
}
