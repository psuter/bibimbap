package bibimbap

import akka.actor._

trait Module extends Actor {
  val logger: ActorRef
  val repl: ActorRef

  val name: String

  val keywords = List[String]()

}
