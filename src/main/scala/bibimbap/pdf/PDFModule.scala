package bibimbap
package pdf

import java.io.File

import org.apache.pdfbox._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.util._

class PDFModule(settings : Settings) extends Module(settings) {
  module =>

  val name = "PDF fulltext import"

  val keyword = "pdf"

  override val moreActions = Seq(importAction)

  private val info : Any=>Unit = settings.logger.info
  private val warn : Any=>Unit = settings.logger.warn

  lazy val importAction = new FileAction[Unit]("import") {
    def warn(msg : Any) = module.warn(msg)

    val description = "Import a fulltext PDF into local library."

    def run(file : File) : Unit = {
      val document = PDDocument.load(file)

      val nfo = document.getDocumentInformation()

      def p(t : String, f : PDDocumentInformation=>Any) : Unit = {
        info("Info [" + t + "] : " + f(nfo))
      }
      p("title", _.getTitle())
      p("author", _.getAuthor())

      val stripper = new PDFTextStripper()
      val text = stripper.getText(document)

      println(text) 
 
      println("OK !")
    }
  }
}
