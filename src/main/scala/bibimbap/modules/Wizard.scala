package bibimbap
package modules

import akka.actor._
import bibtex._
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
    val entry   = res.entry
    var map     = entry.entryMap
    var key     = entry.key
    var kind    = entry.tpe

    val allStdFields     = BibTeXEntryTypes.allStdFields
    val meaningFulFields = entry.stdFields

    def inBold(str: String): String    = if (settings.colors) Console.BOLD+str+Console.RESET else str
    def inRedBold(str: String): String = if (settings.colors) Console.BOLD+Console.RED+str+Console.RESET else str

    def display() {
      entry.display(console ! Out(_), inBold, inRedBold)
    }

    display()

    var continue = true;
    var cancel   = false;

    while(continue && !cancel) {
      syncMessage[LineRead](console, ReadLineWithHandle("edit field> ")) match {
        case Some(LineRead("" | "exit")) | Some(EOF) | None =>
          continue = false;

        case Some(LineRead("c")) =>
          cancel = true

        case Some(LineRead("d")) =>
          display()

        case Some(LineRead(field)) =>
          val fieldContent = map.get(field).map(_.toJava)

          val newValue = syncMessage[LineRead](console, ReadLineWithHandle("edit value> ", fieldContent)) match {
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
              if (!allStdFields(field) || meaningFulFields(field)) {
                map += field -> newValue.get
              } else {
                console ! Error("Invalid field "+field+" for entry of type "+entry.tpe)
              }
            }
        case _ =>
      }
    }


    if (cancel) {
      console ! Success("Edit cancelled!")
      res
    } else {
      BibTeXEntry.fromEntryMap(kind, key, map, console ! Error(_)) match {
        case Some(entry) =>
          if (entry != res.entry) {
            console ! Success("Entry edited!")
            res.copy(entry = entry, isEdited = true)
          } else {
            console ! Success("Entry not modified!")
            res
          }
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
