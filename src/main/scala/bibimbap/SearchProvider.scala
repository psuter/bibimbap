package bibimbap

import akka.actor._

trait SearchProvider extends Actor {
  val source: String

  def receive: Receive = {
    case Search(terms, limit) =>
      sender ! search(terms, limit)

    case ImportedResult(res) =>
      onImport(res)
      // no message back
    case _ =>
  }

  def search(terms: List[String], limit: Int): SearchResults

  def onImport(res: SearchResult) = {}
}
