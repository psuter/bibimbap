package bibimbap

import akka.actor._
import jline._
import java.io.File

class Console(repl: ActorRef, settings: Settings, historyFileName: String) extends Actor with ActorHelpers {
  val console = self

  private def out(msg: String) {
    Console.println(msg)
  }

  var modules = List[ActorRef]()

  val reader    = new ConsoleReader
  val completor = new Completor {
    def complete(buffer: String, pos: Int, results: java.util.List[_]): Int = {
      import collection.JavaConversions._

      val resultsList = results.asInstanceOf[java.util.List[String]]

      var ind = -1

      for (Completed(candidates, index) <- dispatchMessage[Completed](Complete(buffer, pos), modules) if candidates != Nil) {
        if (ind == -1) {
          ind = index
        }

        if (index == ind) {
          resultsList addAll candidates
        }
      }

      ind
    }
  }

  override def preStart() {
    val history = new History(new File(historyFileName))
    reader.setHistory(history)

    reader.addCompletor(completor)
  }

  private val defaultHandle = "bibimbap> "

  def receive = {
    case OnStartup(modules) =>
      this.modules = modules.values.toList
      sender ! CommandSuccess

    case ReadLine =>
      out("")
      sender ! LineRead(reader.readLine(defaultHandle))
      out("")
    case ReadLineWithHandle(handle, oline) =>
      sender ! LineRead(reader.readLine(handle))
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
