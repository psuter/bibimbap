package bibimbap

trait Settings {
  val logger : Logger

  def get(moduleName : String, key : String) : Option[String]
}

object DefaultSettings extends Settings {
  val logger = new Logger {
    def info(any : Any) : Unit = {
      Console.println(any)
    }

    def warn(any : Any) : Unit = {
      Console.println(Console.RED + any + Console.RESET)
    }
  }

  def get(moduleName : String, key : String) : Option[String] = None
}
