package bibimbap
package modules

import akka.actor._
import data._

class SearchLocal(val repl: ActorRef, val logger: ActorRef) extends SearchModule {
  val name = "SearchLocal"

  def search(terms: List[String]): SearchResults = {
    Nil
  }
}
