package bibimbap

trait Settings {
  val logger : Logger

  def apply(moduleName : String, key : String) : String =
    get(moduleName, key).get

  def get(moduleName : String, key : String) : Option[String]
}

object DefaultSettings extends Settings {
  private val sep = System.getProperty("file.separator")
  private lazy val home = System.getProperty("user.home") + sep
  private lazy val defaultDirCache = home + ".bibimbap" + sep + "cache"

  val logger = new Logger {
    def info(any : Any) : Unit = {
      Console.println(any)
    }

    def warn(any : Any) : Unit = {
      Console.println(Console.RED + any + Console.RESET)
    }
  }

  def get(m : String, k : String) : Option[String] = m match {
    case "general" => k match {
      case "bib.filename" => Some("managed.bib")
      case "dir.cache" => Some(defaultDirCache)
      case _ => None
    }
    case _ => None
  }
}
