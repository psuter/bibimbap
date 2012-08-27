package bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.util.duration._
import scala.concurrent.ExecutionContext

trait ActorHelpers extends Actor {
  implicit val timeout: Timeout = Timeout(5.seconds)

  val logger: ActorRef

  def dispatchCommand[T: ClassTag](cmd: Any, to: List[ActorRef])(implicit timeout: Timeout): List[T] = {
    val futures = to.map(actor => (actor ? cmd).mapTo[T].map(Some(_)).recover {
      case e: java.util.concurrent.TimeoutException =>
        logger ! Error("Timeout while waiting on partial results")
        None
      case c: ClassCastException =>
        logger ! Error("Unnexpected return value from command: "+c.getMessage)
        None
    })

    try {
      Await.result(Future.sequence(futures), timeout.duration).flatten
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        logger ! Error("Timeout while waiting for command result")
        List()
    }
  }

  def syncCommand(actor: ActorRef, cmd: Any): Option[CommandResult] = {
    dispatchCommand[CommandResult](cmd, List(actor)).headOption
  }

  def syncMessage[T: ClassTag](actor: ActorRef, cmd: Any): Option[T] = {
    dispatchCommand[T](cmd, List(actor)).headOption
  }
}
