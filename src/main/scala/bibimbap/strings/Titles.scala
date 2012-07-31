package bibimbap
package strings

class Titles(settings : Settings) extends Module(settings) {
  module =>

  val name = "Parsing all titles from DBLP"

  val keyword = "titles"

  override val moreActions = Seq(parse)

  private lazy val parse = new Action[Unit]("parse") {
    val description = "Bla bla"

    import java.io._

    def run(args : String*) : Unit = {
      val br = new BufferedReader(new FileReader("titles"))
      val bw = new BufferedWriter(new FileWriter("cleans"))

      var line = ""

      while({ line = br.readLine; line } != null) {
        val newLine = cleanupTitle(line)
        bw.write(newLine)
        bw.newLine()        
      }
    }
  }

  private val FinalDot = """(.*)\.\s*""".r
  private def cleanupTitle(title : String) : String = {
    import strings.StringUtils

    val trimmed = title.trim
    val noDot = trimmed match {
      case FinalDot(s) => s
      case other => other
    }

    StringUtils.unescapeHTML(noDot)
  }
}
