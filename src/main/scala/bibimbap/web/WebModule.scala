package bibimbap
package web

import bibimbap.data._
import org.apache.commons.httpclient._, methods._, params._, cookie._
import org.apache.commons.io.IOUtils

class WebModule(settings : Settings) extends Module(settings) {
  val name = "Web DBLP access"

  val keyword = "web"

  override def searchAction = Some(webSearchAction)

  val webSearchAction = new Action[SearchResult]("search") {
    val description = "Search for records on DBLP."

    def run(args : String*) : SearchResult = {
      val pattern = args.mkString(" ")

      val url = "http://www.dblp.org/search/api/?q=%s&h=10&c=4&f=0&format=json"

      val pattern_enc = java.net.URLEncoder.encode(pattern, "UTF-8");
      val str = httpRequestToString(String.format(url, pattern_enc))

      findJsonURLs(str) flatMap extractFromUrl _
    }
  }

  def findJsonURLs(str: String): List[String] = {
      import scala.util.parsing.json._

      JSON.parseFull(str) match {
        case Some(data) =>
          val results = data.asInstanceOf[Map[String, Any]]("result")
                            .asInstanceOf[Map[String, Any]]("hits")
                            .asInstanceOf[Map[String, List[Map[String, Any]]]]("hit");


          val res = for (res <- results) yield {
            try {
              val url = res("url").asInstanceOf[String]

              Some(url)
            } catch {
              case _ =>
                settings.logger.warn("Unable to handle: "+res)
                None
            }
          }

          res.flatten
      case _ =>

        Nil
      }
  }

  val reBlock = """(?s)<pre>@(.+?)\{.+?>:([^,]+),(.+?)</pre>""".r
  val reParts = """(?s)\s+([a-z]+)\s+=\s+\{(.+?)\}(,|\s+\})""".r
  val reWS    = """\s+""".r

  def extractFromUrl(url: String): Option[SearchResultEntry] = {
    val str = httpRequestToString(url)

    var fullData = Map[String, String]()

    for (m <- reBlock.findAllIn(str).matchData) {
      val tpe = m.group(1)
      val ref = m.group(2)
      val content = m.group(3)

      // Extract parts of content:

      var data = reParts.findAllIn(content).matchData.map{ m =>
      
        (m.group(1) -> reWS.replaceAllIn(m.group(2), " "))
      } toMap

      data += "type" -> tpe
      data += "ref" -> ref

      for ((k, v) <- data) {
        if (!fullData.contains(k)) {
          fullData += k -> v
        }
      }
    }

    val optEntry = fullData("type") match {
      case "inproceedings" =>
        Some(new InProceedings(
          fullData.get("author").map(s => s.split(" and ").toSeq).getOrElse(Nil),
          fullData.getOrElse("title", "N/A"),
          fullData.getOrElse("booktitle", "N/A"),
          fullData.getOrElse("year", "0").toInt,
          fullData.get("editor").map(s => s.split(" and ").toSeq).getOrElse(Nil),
          fullData.get("series"),
          fullData.get("pages"),
          fullData.get("organization"),
          fullData.get("publisher"),
          fullData.get("address"),
          fullData.get("month"),
          fullData.get("note"),
          fullData.get("key")
          ))

      case "article" =>
        Some(new Article(
          fullData.get("author").map(s => s.split(" and ").toSeq).getOrElse(Nil),
          fullData.getOrElse("title", "N/A"),
          fullData.getOrElse("journal", "N/A"),
          fullData.getOrElse("year", "0").toInt,
          fullData.get("volume"),
          fullData.get("number"),
          fullData.get("pages"),
          fullData.get("month"),
          fullData.get("note"),
          fullData.get("key")
          ))
      case _ =>
        None
    }

    optEntry match {
      case Some(e) =>
        Some(new SearchResultEntry(e, () => e, fullData.get("ee")))
      case None =>
        None
    }
  }


  def httpRequestToString(url: String): String = {
    val res = httpRequest(url)

    IOUtils.toString(res, "UTF-8");
  }


  def httpRequest(url: String): java.io.InputStream  = httpRequest(url, 1);

  def httpRequest(url: String, timeout: Int): java.io.InputStream = {
    val client = new HttpClient()
    val method = new GetMethod(url)

    client.getHttpConnectionManager.getParams.setConnectionTimeout(timeout*1000)

    method.getParams().setParameter(HttpMethodParams.RETRY_HANDLER, new DefaultHttpMethodRetryHandler(3, false))

    client.executeMethod(method)

    method.getResponseBodyAsStream()
  }

  override val moreActions = Seq(webSearchAction)
}
