package bibimbap
package dblp

import org.squeryl._
import org.squeryl.internals._
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import java.sql._

object MySQLMatchAgainst {
  val m1 : OutMapper[Float] = new OutMapper[Float] {
    def doMap(rs : ResultSet) = rs.getFloat(index)
    def sample = 0.0f
  }

  class MatchAgainst(string : String, e : Seq[StringExpression[Option[String]]])
    extends FunctionNode[Float]("<noname>", Some(m1), e)
    with NumericalExpression[Float] {

    override def doWrite(sw: StatementWriter) = {
      sw.write("MATCH")
      sw.write("(")
      sw.writeNodesWithSeparator(args, ",", false)
      sw.write(") AGAINST ('")
      sw.write(string.replace("'", ""))
      sw.write("')")
    }
  }

  def matchAgainst(e : StringExpression[Option[String]]*)(string : String) = new MatchAgainst(string, e.toSeq)
}

