package bibimbap.json

import scala.util.parsing.combinator._

class JsonParser extends JavaTokenParsers {

  private val IsInt = """-?\d+""".r

  def value : Parser[JValue] = obj |
                             arr |
                             stringLiteral ^^ {x => JString(x.substring(1, x.length-1)) } |
                             floatingPointNumber ^^ { x => x match {
                               case IsInt() => JInt(x.toInt)
                               case _ => JDouble(x.toDouble)
                             } } |
                             "null" ^^^ JNull |
                             "true" ^^^ JTrue |
                             "false" ^^^ JFalse

  def obj   : Parser[JObject] = "{"~>repsep(member, ",")<~"}" ^^ {xs: List[(String, JValue)] => JObject(Map[String, JValue]() ++ xs) }

  def arr   : Parser[JArray] = "["~>repsep(value, ",")<~"]" ^^ { x => JArray(x) }

  def member: Parser[(String, JValue)] = stringLiteral~":"~value ^^ { case key ~ _ ~ v => (key.substring(1, key.length-1), v) }


  def parse(content: String): JValue = {
    parseAll(value, content) match {
      case Success(x, _) => x
      case NoSuccess(err, next) =>
        println("failed to parse JSON input (line " + next.pos.line + ", column " + next.pos.column + ")")
        println(content)
        JNull
    }
  }
}

abstract class JValue {
  def \(k: String): JValue;
  def \\(k: String): List[JValue];
}

case class JObject(fields: Map[String, JValue]) extends JValue {
  def \(k: String): JValue = fields.getOrElse(k, JNull)
  def \\(k: String): List[JValue] = fields.get(k).map(List(_)).getOrElse(fields.values.flatMap(_ \\ k).toList)
}

case class JArray(elems: List[JValue]) extends JValue {
  def \(k: String): JValue = elems.headOption.map(_ \ k).getOrElse(JNull)
  def \\(k: String): List[JValue] = elems.flatMap(_ \\ k)
}

abstract class JLit extends JValue {
  def \(k: String): JValue = JNull
  def \\(k: String): List[JValue] = Nil
}

case class JString(v: String) extends JLit
case class JDouble(v: Double) extends JLit
case class JInt(v: Int) extends JLit
case object JTrue  extends JLit
case object JFalse extends JLit
case object JNull  extends JLit
