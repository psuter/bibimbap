package bibimbap

trait Settings {
  val logger : Logger

  val dblpDBType : String
  val dblpDBDSN  : String
  val dblpDBUserName : String
  val dblpDBPassword : String
}

object DefaultSettings extends Settings {
  // val dblpDBType : String = "h2"
  // val dblpDBDSN  : String = "jdbc:h2:/home/psuter/dblph2"
  // val dblpDBUserName : String = "psuter"
  // val dblpDBPassword : String = "abcabc"

  val dblpDBType : String = "MySQL"
  val dblpDBDSN  : String = "jdbc:mysql://localhost/larabib"
  val dblpDBUserName : String = "larabib"
  val dblpDBPassword : String = "Vk23Jn9wK"

  val logger = new Logger {
    def info(any : Any) : Unit = {
      Console.println(any)
    }

    def warn(any : Any) : Unit = {
      Console.println(Console.RED + any + Console.RESET)
    }
  }
}
