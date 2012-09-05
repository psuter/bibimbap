package bibimbap

case class SearchResult(entry: bibtex.BibTeXEntry,
                        sources: Set[String],
                        relevance: Double,
                        isEdited: Boolean = false,
                        oldEntry: Option[bibtex.BibTeXEntry] = None,
                        isManaged: Boolean = false,
                        alternatives: Set[bibtex.BibTeXEntry] = Set()
                      )
