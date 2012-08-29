package bibimbap

case class SearchResult(entry: bibtex.BibTeXEntry, link: Option[String], sources: Set[String])
