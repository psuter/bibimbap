package bibimbap
package modules

import akka.actor._

class General(val repl: ActorRef, val logger: ActorRef, val settings: Settings) extends Module {
  val name = "General"

  def handleCommand: Receive = {
    case Command1("exit") | Command1("quit") =>
      repl ! Shutdown
  }

  val helpItems = Map(
    "exit" -> HelpEntry("exit", "Exits bibimbap"),
    "quit" -> HelpEntry("quit", "Exits bibimbap")
  )
}
