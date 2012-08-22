package bibimbap

import akka.actor._

class Logger(settings: Settings) extends Actor {
  private def out(msg: String) {
    Console.println(msg)
  }

  var store = List[Any]()

  def receive = {
    case LoggerFlush =>
      Console.flush()

      context.become {
        case LoggerFlush =>
          // Ingore
        case LoggerContinue =>
          context.unbecome()
          store.foreach(receive(_))
          store = Nil
        case m => 
          store = m :: store
      }

      sender ! LoggerFlush 

    case LoggerContinue =>
      // Ingore
    case Out(msg: String) =>
      out(msg)
    case Info(msg: String) =>
      out("      "+msg)
    case Warning(msg: String) =>
      out(Console.YELLOW + "  [w]" + Console.RESET + " "+msg)
    case Error(msg: String) =>
      out(Console.RED + "  [!]" + Console.RESET + " "+msg)
    case Success(msg: String) =>
      out(Console.GREEN + "  [\u2713]" + Console.RESET + " "+msg)

  }
}
