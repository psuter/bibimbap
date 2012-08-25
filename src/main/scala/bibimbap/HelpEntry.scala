package bibimbap

import akka.actor._

case class HelpEntry(command: String, short: String, description: List[String] = Nil) {

  def display(to: ActorRef) {
    displayShort(to)
    for (line <- description) {
      to ! Info("  "+line)
    }
  }

  def displayShort(to: ActorRef) {
    to ! Info("%-30s: %s".format(command, short))
  }

}
