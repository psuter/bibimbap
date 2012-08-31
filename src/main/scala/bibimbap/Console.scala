package bibimbap

import akka.actor._
import jline._
import java.io.File

class Console(settings: Settings, historyFileName: String) extends Actor {
  private def out(msg: String) {
    Console.println(msg)
  }

  val reader = new ConsoleReader

  override def preStart() {
    val history = new History(new File(historyFileName))
    reader.setHistory(history)
  }

  var store = List[Any]()

  private val defaultHandle = "bibimbap> "

  def receive = {
    case ReadLine =>
      out("")
      sender ! LineRead(reader.readLine(defaultHandle))
      out("")
    case ReadLineWithHandle(handle, oline) =>
      out("")
      sender ! LineRead(reader.readLine(handle))
      out("")
    case Out(msg: String) =>
      out(msg)
    case Info(msg: String) =>
      out("  "+msg)
    case Warning(msg: String) =>
      if (settings.colors) {
        out(Console.YELLOW + "  [w]" + Console.RESET + " "+msg)
      } else {
        out("  [w] "+msg)
      }
    case Error(msg: String) =>
      if (settings.colors) {
        out(Console.RED + "  [!]" + Console.RESET + " "+msg)
      } else {
        out("  [!] "+msg)
      }
    case Success(msg: String) =>
      if (settings.colors) {
        out(Console.GREEN + "  [\u2713]" + Console.RESET + " "+msg)
      } else {
        out("  [\u2713] "+msg)
      }

  }
}
