package bibimbap
package modules

import akka.actor._
import data._

import strings._

import scala.io.Source

import java.net.URL
import java.net.URLEncoder
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.net.UnknownHostException

import com.codahale.jerkson.Json
import com.codahale.jerkson.ParsingException
import com.codahale.jerkson.AST._

class SearchDBLP(val repl: ActorRef, val logger: ActorRef) extends SearchModule {
  val name = "Search DBLP"

  def search(terms: List[String]): SearchResults = {
    try {
      val pattern = URLEncoder.encode(terms.mkString(" "), "UTF-8")
      val url = new URL(searchURLPrefix + pattern + searchURLPostfix)
      // info("DBLP query URL : [" + url + "].")
      val urlCon = url.openConnection()
      urlCon.setConnectTimeout(3000)
      urlCon.setReadTimeout(3000)
      val content = Source.fromInputStream(urlCon.getInputStream)
      val text = content.getLines.mkString(" ")
      // info("JSON : " + text)
      extractJSONRecords(text).flatMap(recordToResult).toList
    } catch {
      case ce : ConnectException => {
        logger ! Warning("Connection error: " + ce.getLocalizedMessage)
        Nil
      }
      case ste : SocketTimeoutException => {
        logger ! Warning("Network error: " + ste.getLocalizedMessage)
        Nil
      }
      case uhe : UnknownHostException => {
        logger ! Warning("Network error (unknown host): " + uhe.getLocalizedMessage)
        Nil
      }
    }
  }

  private val searchURLPrefix  = "http://www.dblp.org/search/api/?q="
  private val searchURLPostfix = "&h=10&c=4&f=0&format=json"

  private def extractJSONRecords(text : String) : Seq[JValue] = {
    try {
      (Json.parse[JValue](text) \\ "hit").flatMap(hit => hit match {
        case JArray(elems) => elems
        case single : JObject => Seq(single)
        case _ => Nil
      })
    } catch {
      case pe : ParsingException => {
        logger ! Warning("Remote responded with malformed JSON (this is known to happen when there are no results).")
        Nil
      }
    }
  }

  private val unknown : String = "???"

  private val CoRR = """(.*CoRR.*)""".r
  
  // Conference paper entries ("inproceedings")
  private val ConfVenueStr1 = """(.*) (\d\d\d\d):([\d- ]*)""".r
  private val ConfVenueStr2 = """(.*) (\d\d\d\d)""".r

  // Journal entries 
  // e.g. "Commun. ACM (CACM) 55(2):103-111 (2012)"
  private val JourVenueStr1 = """(.*) (\d+)\((\d+)\):([\d- ]*) \((\d\d\d\d)\)""".r
  // e.g. "Acta Inf. (ACTA) 1:271-281 (1972)"
  private val JourVenueStr2 = """(.*) (\d+):([\d- ]*) \((\d\d\d\d)\)""".r
  // e.g. "Logical Methods in Computer Science (LMCS) 4(4) (2008)"
  private val JourVenueStr3 = """(.*) (\d+)\((\d+)\) \((\d\d\d\d)\)""".r

  private def recordToResult(record : JValue) : Option[SearchResult] = {
    def yr2yr(year : Option[String]) : Option[Int] = try {
      year.map(_.trim.toInt)
    } catch {
      case nfe : NumberFormatException => None
    }

    (record \ "title") match {
      case obj : JObject => {
        val authors : Seq[String] = (obj \ "dblp:authors" \ "dblp:author") match {
          case JArray(elems) => elems.collect { case JString(str) => str }
          case JString(single) => Seq(single)
          case _ => Nil
        }

        val title : String = (obj \ "dblp:title" \ "text") match {
          case JString(str) => cleanupTitle(str)
          case _ => unknown
        }

        val link : Option[String] = (obj \ "dblp:title" \ "@ee") match {
          case JString(str) => Some(str)
          case _ => None
        }

        val year : Option[Int] = (obj \ "dblp:year") match {
          case JInt(bigInt) => Some(bigInt.toInt)
          case _ => None
        }

        // Some of the info is entry type specific, so we now check the type.
        (obj \ "dblp:type") match {
          case JString("inproceedings") => {
            val (venue,venueYear,pages) = (obj \ "dblp:venue" \ "text") match {
              case JString(ConfVenueStr1(v, y, p)) => (Some(cleanupVenue(v)), Some(y), Some(cleanupPages(p)))
              case JString(ConfVenueStr2(v, y)) => (Some(cleanupVenue(v)), Some(y), None)
              case JString(os) => logger ! Warning("Could not extract venue information from string [" + os + "]."); (None, None, None)
              case _ => (None, None, None)
            }

            val entry = new InProceedings(
              authors,
              title,
              MString.fromJava(venue.getOrElse(unknown)),
              yr2yr(venueYear).getOrElse(year.getOrElse(0)),
              pages = pages
            )
            Some(SearchResult(entry, link, "DBLP"))
          }

          case JString("article") => {
            // info("In article : " + (obj \ "dblp:venue" \ "text"))
            val (isCoRR,jour,vol,num,pgs,yr) = (obj \ "dblp:venue" \ "text") match {
              case JString(CoRR(_)) => (true, None, None, None, None, None)
              case JString(JourVenueStr1(j,v,n,p,y)) => (false, Some(cleanupJournal(j)), Some(v), Some(n), Some(cleanupPages(p)), Some(y))
              case JString(JourVenueStr2(j,v,p,y)) => (false, Some(cleanupJournal(j)), Some(v), None, Some(cleanupPages(p)), Some(y))
              case JString(JourVenueStr3(j,v,n,y)) => (false, Some(cleanupJournal(j)), Some(v), Some(n), None, Some(y))
              // case JString(os) => warn("Could not extract venue information from string [" + os + "]."); (false, None, None, None, None, None)
              case _ => (false, None, None, None, None, None)
            }

            if(isCoRR) {
              None
            } else {
              val entry = new Article(
                authors,
                title,
                MString.fromJava(jour.getOrElse(unknown)),
                yr2yr(yr).getOrElse(year.getOrElse(0)),
                volume = vol,
                number = num,
                pages = pgs
              )
              Some(SearchResult(entry, link, "DBLP"))
            }
          }

          case JString(other) => {
            // info("Other type : \"" + other + "\"")
            None
          }

          case _ => None
        }
      }
      case _ => None
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

  private def cleanupVenue(venue : String) : String = {
    venue.trim
  }

  private lazy val JournalAbbr = """(.*) \(([A-Z]+)\)""".r
  private def cleanupJournal(journal : String) : String = {
    journal.trim match {
      case JournalAbbr(_, abbr) => abbr
      case other => other
    }
  }

  val Pages = """(\d+)([\s-]+)(\d+)""".r
  private def cleanupPages(pages : String) : String = pages.trim match {
    case Pages(start, _, end) => start + "--" + end
    case other => other
  }
}
