package bibimbap

import akka.actor._

object Main {
  def main(args: Array[String]): Unit = {
    val homeDir = System.getProperty("user.home") + System.getProperty("file.separator")

    val configFileName = homeDir + ".bibimbapconfig"
    val historyFileName = homeDir + ".bibimbaphistory"

    val system  = ActorSystem("bibimbap")

    val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

    val repl = system.actorOf(Props(new Repl(homeDir, configFileName, historyFileName)), name = "repl")

    repl ! Start
  }
}
