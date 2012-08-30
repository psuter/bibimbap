package bibimbap

case class SearchResult(entry: bibtex.BibTeXEntry,
                        sources: Set[String],
                        relevance: Double,
                        isEdited: Boolean = false,
                        isManaged: Boolean = false)
