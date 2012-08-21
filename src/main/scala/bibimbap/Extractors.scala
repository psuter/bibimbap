package bibimbap

abstract class ExCommand(val limit: Int) {
  def unapplySeq(msg: String): Option[Seq[String]] = {
    Some(msg.split("[:,. ]", limit).toList)
  }
}

object CommandL {
  def unapply(msg: String): Option[(String, List[String])] = {
    val wrds = msg.split("[:,. ]", 0).toList
 

    if (wrds.size > 0) {
      Some(wrds.head, wrds.tail)
    } else {
      None
    }
  }
}

object Command1 extends ExCommand(1)
object Command2 extends ExCommand(2)
object Command3 extends ExCommand(3)
object Command4 extends ExCommand(4)
