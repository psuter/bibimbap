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

  private def inBold(str: String): String    = if (settings.colors) Console.BOLD+str+Console.RESET else str
  private def inRedBold(str: String): String = if (settings.colors) Console.BOLD+Console.RED+str+Console.RESET else str

  def doAdd(tpe: BibTeXEntryTypes.BibTeXEntryType): Option[BibTeXEntry] = {

    console ! Out("Entering bibtex entry: "+tpe)

    var fields    = Map[String, MString]()

    val key = getFieldValue("entry key")

    console ! Out("Required fields:")
    for (field <- BibTeXEntryTypes.requiredFieldsFor(tpe).flatMap(_.toFields)) getFieldValue(field) match {
      case Some(v) =>
        fields += field -> MString.fromJava(v)
      case _ =>
    }

    console ! Out("Optional fields:")
    for (field <- BibTeXEntryTypes.optionalFieldsFor(tpe)) getFieldValue(field) match {
      case Some(v) =>
        fields += field -> MString.fromJava(v)
      case _ =>
    }

    BibTeXEntry.fromEntryMap(tpe, key, fields, console ! Error(_))   
  }

  private def displayEntry(entry: BibTeXEntry) {
    entry.display(console ! Out(_), inBold, inRedBold)
  }

  def doEdit(res: SearchResult): SearchResult = {
    val entry   = res.entry
    var tpe     = entry.tpe
    var fields  = entry.entryMap

    val allStdFields     = BibTeXEntryTypes.allStdFields
    val meaningFulFields = entry.stdFields

    console ! Out("Editing BibTeX entry: "+entry.getKey)

    val key = getFieldValue("entry key", Some(entry.getKey))

    console ! Out("Required fields:")
    for (field <- BibTeXEntryTypes.requiredFieldsFor(tpe).flatMap(_.toFields)) getFieldValue(field, fields.get(field).map(_.toJava)) match {
      case Some(v) =>
        fields += field -> MString.fromJava(v)
      case _ =>
    }

    console ! Out("Optional fields:")
    for (field <- BibTeXEntryTypes.optionalFieldsFor(tpe)) getFieldValue(field, fields.get(field).map(_.toJava)) match {
      case Some(v) =>
        fields += field -> MString.fromJava(v)
      case _ =>
    }

    BibTeXEntry.fromEntryMap(entry.tpe, key, fields, console ! Error(_)) match {
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

  val helpItems = Map(
    "edit"   -> HelpEntry("edit <result>",  "Edit the <results>th item from the last search result."),
    "new"    -> HelpEntry("new <kind>",    "Add a new entry of type <kind>.")
  )
}
