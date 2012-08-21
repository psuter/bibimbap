package bibimbap

case object ReadLine


// logger stuff

abstract class LogMsg {
  val msg: String 
};
case class Out(msg: String) extends LogMsg;
case class Error(msg: String) extends LogMsg;
case class Warning(msg: String) extends LogMsg;
case class Info(msg: String) extends LogMsg;
case class Success(msg: String) extends LogMsg;

case class Command(line: String)

abstract class CommandResult
case class CommandError(msg: String) extends CommandResult
case class CommandException(e: Throwable) extends CommandResult
case object CommandSuccess extends CommandResult
case object CommandUnknown extends CommandResult
case object Shutdown extends CommandResult

case object LoggerFlush
case object LoggerContinue

// Search related
case class Search(terms: List[String])
case object Clear
case class Store(results: data.SearchResults)
