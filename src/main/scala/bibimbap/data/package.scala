package bibimbap

import strings._

package object data {
  case class SearchResult(entry : BibTeXEntry, link : Option[String], source : String)
}
