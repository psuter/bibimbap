package bibimbap

import akka.actor._
import akka.util.Timeout
import scala.concurrent.util.duration._

trait Module extends Actor {
  val settings: Settings
  val logger: ActorRef
  val repl: ActorRef

  val name: String

  val keywords = List[String]()

  implicit val timeout = Timeout(15.seconds)
}
