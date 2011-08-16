package bibimbap

abstract class Action[T](val keyword : String) {
  def description : String
  def run(args : String*) : T

  def argAsInt(arg : String) : Option[Int] = {
    try {
      Some(arg.toInt)
    } catch {
      case e : NumberFormatException => None
    }
  }
}
