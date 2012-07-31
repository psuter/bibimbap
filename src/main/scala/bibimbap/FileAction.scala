package bibimbap

import java.io.File

abstract class FileAction[T](kw : String) extends Action[T](kw) {
  // TODO: put all abstract action classes in an "Actions" trait that can be mixed in?
  // Things such as logger etc. could be there using the cake pattern.
  def warn(msg : Any) : Unit

  def description : String
  
  final def run(args : String*) : T = {
    if(args.isEmpty) {
      sys.error("File action " + keyword + " takes an argument.")
    } else {
      val file = new File(args(0))
      if(file.exists && file.canRead) {
        run(file)
      } else {
        sys.error("File " + file + " does not exist or is not readable.")
      }
    }
  }

  def run(file : File) : T 
}
