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
    case Command2("new", tpe) =>
      BibTeXEntryTypes.withNameOpt(tpe).map(doAdd) match {
        case Some(Some(e)) =>
          val res = SearchResult(e, Set("add"), relevance = 1, isEdited = true)

          console ! Out("")
          console ! Success("New entry created!")
          console ! Out("")

          syncCommand(resultsModule, SearchResults(List(res)))
        case None =>

          console ! Error("Invalid entry type!")

        case _ =>
      }

      sender ! CommandSuccess
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

  private def getFieldValue(field: String, value: Option[String] = None): Option[String] = {
    syncMessage[LineRead](console, ReadLine(Some(inBold("%20s").format(field)+" = "), value)) match {
      case Some(LineRead("")) | Some(EOF) | None =>
        None
      case Some(LineRead(value)) =>
        Some(value)
    }
  }

  private def inBold(str: String): String    = settings.BOLD+str+settings.RESET
  private def inRedBold(str: String): String = settings.BOLD+settings.RED+str+settings.RESET

  def doAdd(tpe: BibTeXEntryTypes.BibTeXEntryType): Option[BibTeXEntry] = {

    console ! Out("Entering bibtex entry: "+tpe)

    editFields(tpe, Map(), None)
  }

  private def displayEntry(entry: BibTeXEntry) {
    entry.display(console ! Out(_), inBold, inRedBold)
  }

  def editFields(tpe: BibTeXEntryTypes.BibTeXEntryType,
                 initFields: Map[String, MString],
                 initKey: Option[String]): Option[BibTeXEntry] = {

    var fields           = initFields
    val allStdFields     = BibTeXEntryTypes.allStdFields

    val key = getFieldValue("entry key", initKey)

    console ! Out("")
    console ! Out("  Required fields:")
    for (field <- BibTeXEntryTypes.requiredFieldsFor(tpe).flatMap(_.toFields)) getFieldValue(field, fields.get(field).map(_.toJava)) match {
      case Some(v) =>
        fields += field -> MString.fromJava(v)
      case _ =>
    }

    console ! Out("")
    console ! Out("  Optional fields:")
    for (field <- BibTeXEntryTypes.optionalFieldsFor(tpe)) getFieldValue(field, fields.get(field).map(_.toJava)) match {
      case Some(v) =>
        fields += field -> MString.fromJava(v)
      case _ =>
    }

    BibTeXEntry.fromEntryMap(tpe, key, fields, console ! Error(_))
  }
  
  def doEdit(res: SearchResult): SearchResult = {

    editFields(res.entry.tpe, res.entry.entryMap, Some(res.entry.getKey)) match {
      case Some(entry) =>
        if (entry != res.entry) {
          console ! Success("Entry modified!")
          res.copy(entry = entry, isEdited = true, oldEntry = res.oldEntry.orElse(Some(res.entry)))
        } else {
          console ! Success("Entry not modified!")
          res
        }
      case None =>
        console ! Error("Could not save entry :(")
        res
    }
  }

  val helpItems = Map(
    "edit"   -> HelpEntry("edit <result>",  "Edit the <results>th item from the last search result."),
    "new"    -> HelpEntry("new <kind>",    "Add a new entry of type <kind>.")
  )
}
