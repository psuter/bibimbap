package bibimbap

case class SearchResult(entry: bibtex.BibTeXEntry, sources: Set[String], relevance: Double)
