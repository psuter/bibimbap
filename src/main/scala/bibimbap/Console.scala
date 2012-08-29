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
      sender ! LineRead(reader.readLine(defaultHandle))
    case ReadLineWithHandle(handle) =>
      sender ! LineRead(reader.readLine(handle))
    case Out(msg: String) =>
      out(msg)
    case Info(msg: String) =>
      out("  "+msg)
    case Warning(msg: String) =>
      out(Console.YELLOW + "  [w]" + Console.RESET + " "+msg)
    case Error(msg: String) =>
      out(Console.RED + "  [!]" + Console.RESET + " "+msg)
    case Success(msg: String) =>
      out(Console.GREEN + "  [\u2713]" + Console.RESET + " "+msg)

  }
}
