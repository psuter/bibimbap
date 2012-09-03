package bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import bibtex._

import scala.io.Source

class SearchBibtex(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module
                                                                                      with LuceneRAMBackend
                                                                                      with LuceneSearchProvider {
  val name = "SearchBibtex"

  val source = "searchBibtex"

  override def receive: Receive = {
    case Command2("load", path) =>
      val parser = new BibTeXParser(Source.fromFile(path), console ! Error(_))

      addEntries(parser.entries)

      sender ! CommandSuccess

    case Search(terms) =>
      sender ! search(terms)

    case ImportedResult(res) =>
      // We don't care

    case msg =>
      super[Module].receive(msg)
  }

  private lazy val fileNamesCompletor = new jline.FileNameCompletor()

  override def complete(buffer: String, pos: Int): (List[String], Int) = {
    import collection.JavaConversions._
    if (buffer.startsWith("load ") && pos >= "load ".length) {
      val newbuffer = buffer.substring("load ".length, buffer.length) 
      val newpos    = pos - " load ".length

      val list  = new java.util.ArrayList[String]()
      val index = fileNamesCompletor.complete(newbuffer, newpos, list)

      (list.toList, index + "load ".length)
    } else {
      (Nil, 0)
    }
  }

  val helpItems = Map(
    "load" -> HelpEntry("load <path>",  "loads <path> bibtex file into searchable cache")
  )
}
