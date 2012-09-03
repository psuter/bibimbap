package bibimbap
package modules

import akka.actor._

class General(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "general"

  override def receive: Receive = {
    case Command1("exit") | Command1("quit") =>
      repl ! Shutdown
      sender ! CommandSuccess
    case x =>
      super.receive(x)
  }

  val helpItems = Map(
    "help" -> HelpEntry("help [<command>]", "This help"),
    "exit" -> HelpEntry("exit", "Exits bibimbap"),
    "quit" -> HelpEntry("quit", "Exits bibimbap")
  )
}
