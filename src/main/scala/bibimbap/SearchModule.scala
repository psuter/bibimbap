package bibimbap

import data._

trait SearchModule extends Module {
  def receive = {
    case Search(terms) =>
      search(terms)
      sender ! Nil
    case LastResults(results) =>
      store(results)
    case _ =>
  }

  def search(terms: List[String]): SearchResults

  def store(results: SearchResults) {

  }
}
