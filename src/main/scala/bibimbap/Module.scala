package bibimbap

import bibimbap.data._

abstract class Module(val settings : Settings) {
  import settings.logger.{info,warn}

  // Basic
  val name : String
  val keyword : String

  def requiredSettings : List[String] = Nil

  // Actions
  def moreActions : Seq[Action[_]] = Seq.empty
  final def actions : Seq[Action[_]] = helpAction +: moreActions

  // Submodules
  def subModules : Seq[Module] = Seq.empty

  // Callbacks
  def onImport(sre : SearchResultEntry) : Unit = ()

  final lazy val allSubKeywords : Seq[String] = (actions.map(_.keyword) ++ subModules.map(_.keyword)).sorted

  private def moduleKW = keyword

  val helpAction = new Action[Unit]("help") {
    def description = "List available commands" + (if(moduleKW == "<main>") "." else " for module " + moduleKW + ".")

    def run(args : String*) : Unit = {
      if(!args.isEmpty) {
        warn("help does't take arguments.")
      } else { 
        if(moduleKW != "<main>") {
          info("'" + moduleKW + "' : " + name)
        }
        if(!actions.isEmpty) {
          info("Commands")
          for(a <- actions.sortBy(_.keyword)) {
            info(" - %-10s".format(a.keyword) + " : " + a.description)
          }
        }
        if(!subModules.isEmpty) {
          info("Modules")
          for(m <- subModules.sortBy(_.keyword)) {
            info(" - %-10s".format(m.keyword) + " : " + m.name)
          }
        }
      } 
    }
  }

  final def executeLine(line : String) : Unit = if(line != null) {
    val trimmed = line.trim
    if(trimmed != "") {
      val words = line.trim.split(' ').filterNot(_.isEmpty)
      execute(words : _*)
    }
  }

  final def execute(words : String*) : Unit = {
    if(words.isEmpty) {

    } else {
      val first = words.head
      val rest  = words.tail

      actions.find(_.keyword == first) match {
        case Some(a) => a.run(rest : _*)
        case None => subModules.find(_.keyword == first) match {
          case Some(m) => {
            if(rest.isEmpty) {
              m.helpAction.run()
            } else {
              m.execute(rest : _*)
            }
          }
          case None => warn("Unknown action or module '" + first + "' in module " + moduleKW + ".")
        }
      }
    }
  }
}
