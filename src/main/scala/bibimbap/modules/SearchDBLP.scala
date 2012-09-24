package bibimbap
package modules

import akka.actor._
import bibtex._

import strings._
import util.StringUtils

import scala.io.Source

import java.net.URL
import java.net.URLEncoder
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.net.UnknownHostException
import java.io.IOException

import json._

class SearchDBLP(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends SearchProvider {
  val name   = "Search DBLP"
  val source = "dblp"

  private val searchURLPrefix  = "http://www.dblp.org/search/api/?q="
  private val searchURLPostfix = "&h=10&c=4&f=0&format=json"

  override def search(terms: List[String]): SearchResults = {
    val results = try {
      val pattern = URLEncoder.encode(terms.mkString(" "), "UTF-8")
      val url = new URL(searchURLPrefix + pattern + searchURLPostfix)
      // info("DBLP query URL : [" + url + "].")
      val urlCon = url.openConnection()
      urlCon.setConnectTimeout(10000)
      urlCon.setReadTimeout(10000)
      val content = Source.fromInputStream(urlCon.getInputStream)
      val text = content.getLines.mkString(" ")

      extractJSONRecords(text).flatMap(recordToResult).toList
    } catch {
      case ioe : IOException => {
        console ! Warning("IO error: " + ioe.getLocalizedMessage)
        Nil
      }
      case ce : ConnectException => {
        console ! Warning("Connection error: " + ce.getLocalizedMessage)
        Nil
      }
      case ste : SocketTimeoutException => {
        console ! Warning("Network error: " + ste.getLocalizedMessage)
        Nil
      }
      case uhe : UnknownHostException => {
        console ! Warning("Network error (unknown host): " + uhe.getLocalizedMessage)
        Nil
      }
    }

    SearchResults(results)
  }

  private def extractJSONRecords(text : String) : Seq[JValue] = {
    val jvalue = new JSONParser().parse(text)
    (jvalue \\ "hit").flatMap(hit => hit match {
      case JArray(elems) => elems
      case single : JObject => Seq(single)
      case _ => Nil
    })
  }

  private val unknown : MString = MString.fromJava("???")

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
    def yr2yr(year : Option[String]) : Option[MString] =
      year.map(str => MString.fromJava(str.trim))

    val score = (record \ "@score") match {
      case JInt(x) => x/200d
      case _ => 0d
    }

    val dblpID = (record \ "@id") match {
      case JInt(x) => Some(MString.fromJava(x+""))
      case _ => None
    }

    val optKey = None;

    (record \ "title") match {
      case obj : JObject => {
        val authors : MString = MString.fromJava(((obj \ "dblp:authors" \ "dblp:author") match {
          case JArray(elems) => elems.collect { case JString(str) => str }
          case JString(single) => Seq(single)
          case _ => Nil
        }).mkString(" and "))

        val title : MString = (obj \ "dblp:title" \ "text") match {
          case JString(str) => MString.fromJava(cleanupTitle(str))
          case _ => unknown
        }

        val (link, doi) = (obj \ "dblp:title" \ "@ee") match {
          case JString(str) => 
            val doi = if (str.startsWith("http://doi.acm.org/")) {
              Some(str.substring("http://doi.acm.org/".length, str.length))
            } else if (str.startsWith("http://dx.doi.org/")) {
              Some(str.substring("http://dx.doi.org/".length, str.length))
            } else {
              None
            }

            (Some(MString.fromJava(str)), doi.map(MString.fromJava))
          case _ =>
            (None, None)
        }

        val year : Option[MString] = (obj \ "dblp:year") match {
          case JInt(bigInt) => Some(MString.fromJava(bigInt.toString))
          case _ => None
        }

        // Some of the info is entry type specific, so we now check the type.
        (obj \ "dblp:type") match {
          case JString("inproceedings") => {
            val (venue,venueYear,pages) = (obj \ "dblp:venue" \ "text") match {
              case JString(ConfVenueStr1(v, y, p)) => (Some(cleanupVenue(v)), Some(y), Some(cleanupPages(p)))
              case JString(ConfVenueStr2(v, y)) => (Some(cleanupVenue(v)), Some(y), None)
              case JString(os) => console ! Warning("Could not extract venue information from string [" + os + "]."); (None, None, None)
              case _ => (None, None, None)
            }
            val omap = Map[String, Option[MString]](
                "title"     -> Some(title),
                "author"    -> Some(authors),
                "booktitle" -> venue.map(MString.fromJava),
                "year"      -> yr2yr(venueYear).orElse(year),
                "pages"     -> pages.map(MString.fromJava),
                "link"      -> link,
                "doi"       -> doi,
                "dblp"      -> dblpID
            )

            val map = omap.filterNot(_._2.isEmpty).mapValues(_.get)

            val entry = BibTeXEntry.fromEntryMap(Some(BibTeXEntryTypes.InProceedings), optKey, map, console ! Error(_))

            entry.map(SearchResult(_, Set(source), score))
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
              val omap = Map[String, Option[MString]](
                "author"    -> Some(authors),
                "title"     -> Some(title),
                "journal"   -> jour.map(MString.fromJava),
                "year"      -> yr2yr(yr).orElse(year),
                "volume"    -> vol.map(MString.fromJava),
                "number"    -> num.map(MString.fromJava),
                "pages"     -> pgs.map(MString.fromJava),
                "link"      -> link,
                "doi"       -> doi,
                "dblp"      -> dblpID
              )

              val map = omap.filterNot(_._2.isEmpty).mapValues(_.get)

              BibTeXEntry.fromEntryMap(Some(BibTeXEntryTypes.Article), optKey, map, console ! Error(_)).map(SearchResult(_, Set(source), score))
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
