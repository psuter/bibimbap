package bibimbap
package modules

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.util.concurrent.{Executors, TimeoutException}
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.util.duration._
import scala.concurrent.ExecutionContext

import data._

class Search(val repl: ActorRef, val logger: ActorRef) extends Module {
  val name = "Search"

  var lastSearch = List[SearchResult]()

  val searchModules = List(
    context.actorOf(Props(new SearchLocal(repl, logger)), name = "SearchLocal"),
    context.actorOf(Props(new SearchDBLP(repl, logger)),  name = "SearchDBLP")
  )

  def receive = {
    case Command(CommandL("search", args)) =>
      search(args)
      sender ! CommandSuccess
    case _ =>
      sender ! CommandUnknown
  }

  def search(args: List[String]) = {
    logger ! Info("Searching for "+args)

    val futures = searchModules.map(actor => (actor ? Search(args)).mapTo[SearchResult] recover {
      case e: Throwable => Nil
    })

    try {
      val results = Await.result(Future.sequence(futures), 15.seconds)
      println(results)
    } catch {
      case e: TimeoutException =>
        logger ! Error("Failed to gather search results in time")
    }
  }
}
