package bibimbap

import akka.actor.ActorRef

import io.Source

import java.net.URL
import java.net.URLEncoder
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.net.UnknownHostException
import java.io.{IOException, InputStream}

import scala.xml._

trait WebProvider {
  val console: ActorRef

  def HTTPQueryAsString(url: String): Option[String] = {
    HTTPQuery(url).map(Source.fromInputStream(_).getLines.mkString(" "))
  }

  def HTTPQueryAsXML(url: String): Option[Elem] = {
    HTTPQuery(url).map(XML.load(_))
  }

  private def HTTPQuery(url: String): Option[InputStream] = {
    try {
      val jurl = new URL(url)
      val urlCon = jurl.openConnection()
      urlCon.setConnectTimeout(10000)
      urlCon.setReadTimeout(10000)
      Some(urlCon.getInputStream)
    } catch {
      case ioe : IOException => {
        console ! Warning("IO error: " + ioe.getLocalizedMessage)
        None
      }
      case ce : ConnectException => {
        console ! Warning("Connection error: " + ce.getLocalizedMessage)
        None
      }
      case ste : SocketTimeoutException => {
        console ! Warning("Network error: " + ste.getLocalizedMessage)
        None
      }
      case uhe : UnknownHostException => {
        console ! Warning("Network error (unknown host): " + uhe.getLocalizedMessage)
        None
      }
    }
  }
}
