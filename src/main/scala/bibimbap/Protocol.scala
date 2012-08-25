package bibimbap

import akka.actor.ActorRef

case object Start
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

trait Command
case class InputCommand(line: String) extends Command
case class OnStartup(modules: Map[String, ActorRef]) extends Command;
case class OnShutdown() extends Command;

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

case class StoreResults(results: data.SearchResults)
case class GetResults(index: String)
case object ShowResults

