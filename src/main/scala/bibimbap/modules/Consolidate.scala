package bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import io.Source
import java.io.{FileWriter, File}

class Consolidate(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "consolidate"

  override val dependsOn = Set("search")

  lazy val searchModule = modules("search")

  private var consolidater    = sender
  private var consolidatePath = ""
  private var entries         = Stream[BibTeXEntry]()
  private var entriesCons     = Map[BibTeXEntry, Option[BibTeXEntry]]()

  def postfixPath(path: String, postfix: String): String = {
    val newPath = path.replaceAll("\\.bib$", postfix+".bib")

    if (newPath == path) {
      path+postfix
    } else {
      newPath
    }
  }

  def normal: Receive = {
    case Command2("consolidate", path) =>
      // First we load entries from path
      try {
        val parser      = new BibTeXParser(Source.fromFile(path), console ! Warning(_))
        consolidatePath = postfixPath(path, "-consolidated")
        entries         = parser.entries
        entriesCons     = Map()
        consolidater    = sender

        if (entries.size > 0) {
          context.become(consolidating)
          for (entry <- entries) {
            searchModule ! SearchSimilar(entry)
          }
        } else {
          finishConsolidation()
        }

      } catch {
        case e: Throwable =>
          sender ! CommandException(e)
          context.become(normal)
      }

    case Command2("lint", path) =>
      try {
        val parser      = new BibTeXParser(Source.fromFile(path), console ! Warning(_))
        val newPath     = postfixPath(path, "-lint")

        val fw = new FileWriter(new File(newPath), false)

        for (entry <- parser.entries) {
          fw.write(entry.toString)
          fw.write("\n\n")
        }

        fw.close

        console ! Success("Reformatted file saved to "+newPath)

        sender ! CommandSuccess
      } catch {
        case e: Throwable =>
          sender ! CommandException(e)
      }

    case x =>
      super.receive(x)
  }

  def consolidating: Receive = {
    case SimilarEntry(oldEntry, optNewEntry) =>

      entriesCons += oldEntry -> optNewEntry

      if (entries.size > 100 && (entriesCons.size % 40 == 0)) {
        val progress = (entriesCons.size*100d)/entries.size
        console ! Out("   "+("%3d".format(progress.toInt))+"% ("+entriesCons.size+"/"+entries.size+")")
      }

      if (entriesCons.size == entries.size) {
        finishConsolidation()
      }
  }

  def finishConsolidation() {
    var modified = 0

    val results = for (entry <- entries) yield {
      val entr = entriesCons(entry) match {
        case Some(newEntry) if newEntry like entry =>
          var fields = entry.entryMap

          for ((k, v) <- newEntry.entryMap if !fields.contains(k)) {
            fields += k -> v
          }
          BibTeXEntry.fromEntryMap(entry.tpe, entry.key, fields, console ! Error(_)).getOrElse(entry)
        case _ =>
          entry
      }

      if (entr != entry) {
        modified += 1
      }

      entr
    }


    try {
      val fw = new FileWriter(new File(consolidatePath), false)

      for (entry <- results) {
        fw.write(entry.toString)
        fw.write("\n\n")
      }

      fw.close

      console ! Success("Modified "+modified+" entries.")
      console ! Success("Consolidated file saved to "+consolidatePath)

      consolidater ! CommandSuccess
    } catch {
      case e: Throwable =>
        consolidater ! CommandException(e)
    }
    context.become(normal)
  }

  override def receive: Receive = normal

  override def complete(buffer: String, pos: Int): (List[String], Int) = {
    val Lint        = FileCompletor("lint ")
    val Consolidate = FileCompletor("consolidate ")

    (buffer, pos) match {
      case Lint(alts, pos) =>
        (alts, pos)
      case Consolidate(alts, pos) =>
        (alts, pos)
      case _ =>
        (Nil, 0)
    }
  }

  val helpItems = Map(
    "consolidate" -> HelpEntry("consolidate <path>", "Iteratively consolidate the entries found in <path>."),
    "lint"        -> HelpEntry("lint <path>",        "Parse and pretty-print <path>.")
  )
}
