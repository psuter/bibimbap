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

// Interaction between the REPL and modules
trait Command
case class InputCommand(line: String) extends Command
case class OnStartup(modules: Map[String, ActorRef]) extends Command;
case class OnShutdown() extends Command;
case object Shutdown extends Command;

abstract class CommandResult
case class CommandError(msg: String) extends CommandResult
case class CommandException(e: Throwable) extends CommandResult
case object CommandSuccess extends CommandResult
case object CommandUnknown extends CommandResult

// Interation between the REPL and the logger module
case object LoggerFlush
case object LoggerContinue

// Protocol to/from search module
//  => Search(terms)
//  <= SearchResults(...)
case class Search(terms: List[String])

//  => SearchOne(terms)
//  <= SearchResults(Nil  |  x :: Nil)
case class SearchOne(terms: List[String])

case class SearchResults(entries: List[data.SearchResult])


// Protocol from/to results module
// => GetResults(index)
// <= SearchResults(...)
case class GetResults(index: String)

// => ShowResults
// <= CommandSuccess
case object ShowResults

