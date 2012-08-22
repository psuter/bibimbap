package bibimbap

import data._
import akka.actor._

trait SearchModule extends Actor {
  val source: String

  def receive = {
    case Search(terms) =>
      sender ! search(terms)
    case Clear =>
      clear()
    case Store(results) =>
      store(results)
    case _ =>
  }

  def search(terms: List[String]): SearchResults

  def store(results: SearchResults) { }

  def clear() { }
}
