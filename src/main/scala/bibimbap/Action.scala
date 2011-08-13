package bibimbap

trait Action[T] {
  val keyword : String
  def description : String
  def run(args : String*) : T
}
