package bibimbap

import akka.actor._
import jline._
import java.io.{File, FileInputStream, FileDescriptor, PrintWriter, OutputStreamWriter}

class Console(repl: ActorRef, settings: Settings, historyFileName: String) extends Actor with ActorHelpers {
  val console = self

  private def out(msg: String) {
    Console.println(msg)
  }

  var modules = List[ActorRef]()

  var buffer: Option[String] = None

  val terminal = Terminal.setupTerminal() match {
    case u: UnixTerminal =>
      new UnixTerminal {
        override def isSupported = {
          magicTrick()
          super.isSupported
        }
      }
    case w: WindowsTerminal =>
      new WindowsTerminal {
        override def isSupported = {
          magicTrick()
          super.isSupported
        }
      }

    case u: UnsupportedTerminal =>
      new UnsupportedTerminal {
        override def isSupported = {
          magicTrick()
          super.isSupported
        }
      }
  }

  val reader    = new ConsoleReader(new FileInputStream(FileDescriptor.in), 
                                     new PrintWriter(new OutputStreamWriter(Console.out,
                                      System.getProperty("jline.WindowsTerminal.output.encoding", System.getProperty("file.encoding")))), null, terminal)


  private def magicTrick(): Unit = buffer match {
    case Some(b) =>
      reader.putString(b)
      buffer = None
    case _ =>
  }

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

    case ReadLine(handle, oline) =>
      buffer = oline
      sender ! LineRead(reader.readLine(handle.getOrElse(defaultHandle)))
    case Out(msg: String) =>
      out(msg)
    case Info(msg: String) =>
      out("  "+msg)
    case Warning(msg: String) =>
      if (settings.colors) {
        out(Console.YELLOW + "  \u27A4" + Console.RESET + "  "+msg)
      } else {
        out("  [warn] "+msg)
      }
    case Error(msg: String) =>
      if (settings.colors) {
        out(Console.RED + "  \u27A4" + Console.RESET + "  "+msg)
      } else {
        out("  [err] "+msg)
      }
    case Success(msg: String) =>
      if (settings.colors) {
        out(Console.GREEN + "  \u27A4" + Console.RESET + "  "+msg)
      } else {
        out("  [ok] "+msg)
      }

  }
}
