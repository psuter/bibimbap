package bibimbap

import data._
import akka.actor._

trait SearchModule extends Actor {
  val source: String

  def receive = {
    case Search(terms) =>
      sender ! search(terms)

    case ImportedResult(res) =>
      onImport(res)
      // no message back
    case _ =>
  }

  def search(terms: List[String]): SearchResults

  def onImport(res: SearchResult) = {}
}
