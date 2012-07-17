package bibimbap

import scala.io.Source
import java.io.File

import scala.collection.mutable.{Map=>MutableMap}

class ConfigFileParser(filename : String) {
  import DefaultSettings.logger.{info,warn}

  def parse : Option[Settings] = {
    val file = new File(filename)
    if(!file.exists) return None

    if(!file.canRead) {
      warn("Cannot read file : " + filename + ". Check read rights.")
      return None
    }

    try {
      val source = Source.fromFile(filename)

      var inModule : Option[String] = None
      val sets : MutableMap[(String,String),String] = MutableMap.empty

      val ModuleDef = """\[([a-zA-Z]+)\]""".r
      val KeyValDef = """([a-zA-Z\.]+)\s*=\s*([^\s].*)""".r

      for(line <- source.getLines) {
        line.trim match {
          case "" => ;
          case comment if comment.startsWith("#") => ;
          case ModuleDef(modName) => inModule = Some(modName)
          case KeyValDef(k, v) => inModule match {
            case None => warn("In config file : Ignoring line outside of module definition : ``" + k + " = " + v + "''.")
            case Some(m) => {
              sets((m,k)) = v
            }
          }
          case elze => warn("In config file : Ignoring line : ``" + elze + "''.")
        }
      }

      Some(new Settings {
        val logger = DefaultSettings.logger
        override def get(moduleName : String, key : String) : Option[String] = {
          sets.get((moduleName, key)) match {
            case s @ Some(_) => s
            case None => DefaultSettings.get(moduleName, key)
          }
        }
      })
    } catch {
      case anything => warn(anything.getMessage); None
    }
  }
}
