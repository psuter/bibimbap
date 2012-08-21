package bibimbap
package modules

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.util.duration._
import scala.concurrent.ExecutionContext

class Search(val repl: ActorRef, val logger: ActorRef) extends Module {
  val name = "Search"

  val searchModules = List[ActorRef](

  )

  def receive = {
    case Command(line) =>
      line match {
        case CommandL("search", args) =>
          search(args)
          sender ! CommandSuccess
        case _ =>
          sender ! CommandUnknown
      }
  }

  def search(args: List[String]) = {
    logger ! Info("Searching for "+args)
  }
}
