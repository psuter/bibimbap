package bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import bibtex._

import scala.io.Source

class SearchBibtex(val repl: ActorRef, val console: ActorRef, val settings: Settings, val path: String) extends LuceneRAMBackend
                                                                                                        with LuceneSearchProvider {
  val name = "SearchBibtex"

  val source = "bibtex - "+path

  override def receive: Receive = {
    case Start =>
      try {
        val parser = new BibTeXParser(Source.fromFile(path), console ! Warning(_))
        addEntries(parser.entries)
        sender ! CommandSuccess
      } catch {
        case e: Throwable =>
          sender ! CommandException(e)
      }
    case x =>
      super.receive(x)
    }


}
