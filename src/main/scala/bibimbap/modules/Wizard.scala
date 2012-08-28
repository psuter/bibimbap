package bibimbap
package modules

import akka.actor._
import data._
import strings._

class Wizard(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "wizard"

  override val dependsOn = Set("results")

  lazy val resultsModule = modules("results")

  override def receive: Receive = {
    case Command2("edit", ind) =>
      syncMessage[SearchResults](resultsModule, GetResults(ind)) match {
        case Some(SearchResults(res)) =>
          val newResults = res.map(doEdit)

          syncCommand(resultsModule, ReplaceResults(ind, newResults))
        case _ =>
          console ! Error("Invalid index")
      }

      sender ! CommandSuccess

    case x =>
      super.receive(x)
  }

  def doEdit(res: SearchResult): SearchResult = {
    console ! Out("Editing: ")

    var map = res.entry.entryMap

    def display(map: Map[String, MString]) {
      for ((k, v) <- map) {
        console ! Out("  %12s = %s".format(k, v))
      }
    }

    display(map)

    console ! Out("What field to edit (d to display, c to cancel)?")

    var continue = true;
    var cancel   = false;

    while(continue && !cancel) {
      syncMessage[LineRead](console, ReadLineWithHandle("edit field> ")) match {
        case Some(LineRead("" | "exit")) | Some(EOF) | None =>
          continue = false;

        case Some(LineRead("c")) =>
          cancel = true

        case Some(LineRead("d")) =>
          display(map)

        case Some(LineRead(field)) if map contains field =>
          val newValue = syncMessage[LineRead](console, ReadLineWithHandle("edit value> ")) match {
            case Some(LineRead("")) =>
              None

            case Some(LineRead(line)) =>
              Some(MString.fromJava(line))

            case _ =>
              None
          }

          if (newValue.isEmpty) {
            map -= field
          } else {
            map += field -> newValue.get
          }
        case Some(LineRead(field)) =>
          console ! Error("Unknown field: "+field)
        case _ =>
      }
    }


    if (cancel) {
      console ! Success("Edit cancelled!")
      res
    } else {
      BibTeXEntry.fromEntryMap(map, console ! Error(_)) match {
        case Some(entry) =>
          console ! Success("Entry edited!")
          res.copy(entry = entry)
        case None =>
          console ! Error("Could not save entry :(")
          res
      }
    }
  }

  val helpItems = Map(
    "edit"   -> HelpEntry("edit <result>",  "Edit the <results>th item from the last search result.")
  )
}
