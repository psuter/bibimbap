package bibimbap
package modules

import akka.actor._
import data._
import strings._

class Managed(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module
                                                                                    with SearchProvider
                                                                                    with LuceneRAMBackend
                                                                                    with LuceneSearchProvider {
  val name = "Managed"

  val source = "managed"

  override def preStart = {
    // Load managed.bib into lucene
  }

  override def receive: Receive = {
    case Search(terms) =>
      sender ! search(terms)

    case ImportedResult(res) =>
      onImport(res)
      // no message back
    case _ =>
  }

  val helpItems = Map(
    "import" -> HelpEntry("import <result>",  "Imports the <result>th item from the last search results into managed.bib")
  )
}
