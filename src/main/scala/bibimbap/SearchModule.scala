package bibimbap

import data._
import akka.actor._

trait SearchModule extends Actor {
  val source: String

  def receive = {
    case Search(terms) =>
      sender ! doSearch(terms)

    case ImportedResult(res) =>
      doImport(res)
      // no message back
    case _ =>
  }

  def doSearch(terms: List[String]): SearchResults

  def doImport(res: SearchResult) = {}
}
