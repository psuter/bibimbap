package bibimbap

trait Settings {
  def apply(moduleName : String, key : String) : String =
    get(moduleName, key).get

  def get(moduleName : String, key : String) : Option[String]

  var colors = get("general", "colors").map(_ == "yes").getOrElse(true)



  // Helpers for colors
  val RED       = if(colors) Console.RED        else ""
  val BLUE      = if(colors) Console.BLUE       else ""
  val BOLD      = if(colors) Console.BOLD       else ""
  val YELLOW    = if(colors) Console.YELLOW     else ""
  val GREEN     = if(colors) Console.GREEN      else ""
  val MAGENTA   = if(colors) Console.MAGENTA    else ""
  val RESET     = if(colors) Console.RESET      else ""
}

object DefaultSettings extends Settings {
  private val sep = System.getProperty("file.separator")
  private lazy val home = System.getProperty("user.home") + sep
  private lazy val defaultDirCache = home + ".bibimbap" + sep + "cache"

  def get(m : String, k : String) : Option[String] = m match {
    case "general" => k match {
      case "bib.filename" => Some("managed.bib")
      case "dir.cache" => Some(defaultDirCache)
      case _ => None
    }
    case _ => None
  }
}
