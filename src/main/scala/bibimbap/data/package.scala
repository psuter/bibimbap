package bibimbap

package object data {
  // The idea of the pair of entry, ()=>entry is that in some cases it is
  // cheaper to get an incomplete version of the entry which is still
  // sufficient for the purpose of displaying search results. The callback is
  // used to "precise" the entry once it is examined or imported.
  case class SearchResultEntry(entry : BibTeXEntry, callback : ()=>BibTeXEntry, link : Option[String], source : String)
  type SearchResult = Iterable[SearchResultEntry]

  private[data] def forConsistency(msg : String)(expr : =>Boolean)(implicit entryType : BibTeXEntryTypes.BibTeXEntryType) {
    if(!expr) {
      throw new InconsistentBibTeXEntry("Error in " + entryType + ": " + msg + ".")
    }
  }

  // Approximates the string with a version that matches [a-zA-Z]*.
  private[data] def strToASCII(str : String) : String = str.flatMap(accentRemove)

  // Produces an equivalent string that is valid LaTeX (in theory)
  private[data] def strToLaTeX(str : String) : String = latexify(str)

  private val commonWords = Set("", "in", "the", "a", "an", "of", "for", "and", "or", "by", "on", "with")
  private def camelcasify(str : String) : Seq[String] = {
    str.split(" ")
      .map(strToASCII)
      .filterNot(_.isEmpty)
      .map(_.toLowerCase)
      .filterNot(commonWords)
      .map(_.capitalize)
  }

  private[data] def entryToKey(entry : BibTeXEntry) : String = {
    val persons   = if(!entry.authors.isEmpty) entry.authors else entry.editors
    val lastnames = if(persons.size > 3) {
      strToASCII(persons(0).split(" ").last) + "ETAL"
    } else {
      strToASCII(persons.map(ss => ss.split(" ").last).mkString(""))
    }

    val yr = entry.year match {
      case Some(y) => {
        val last = y % 100
        if(last < 10) "0" + last else last.toString
      }
      case None => ""
    }
    val title = entry.title match {
      case Some(t) => camelcasify(t).take(6).mkString("")
      case None => ""
    }

    lastnames + yr + title
  }

  // Tries to shorten the first names (whatever that means).
  private def shortenName(name : String) : String = {
    val elements = name.split(" ").filterNot(_.isEmpty)
    elements.dropRight(1).map(e => e(0) + ".").mkString("") + elements.last
  }

  private[data] def entryToInline(entry : BibTeXEntry) : String = {
    val persons = if(!entry.authors.isEmpty) {
      (if(entry.authors.size > 4) {
        shortenName(entry.authors.head) + " et al."
      } else {
        entry.authors.map(shortenName(_)).mkString(", ")
      })
    } else if(!entry.editors.isEmpty) {
      (if(entry.editors.size > 4) {
        shortenName(entry.editors.head) + " et al."
      } else {
        entry.editors.map(shortenName(_)).mkString(", ")
      }) + "ed."
    } else {
      "?"
    }

    val title = "\"" + entry.title.getOrElse("?") + "\""

    val where = 
      entry.booktitle.getOrElse(
        entry.journal.getOrElse(
          entry.school.getOrElse(
            entry.howpublished.getOrElse("?"))))

    val year = entry.year.map(_.toString).getOrElse("?")

    persons + ", " + title + ", " + where + ", " + year
  }

  private[data] def entryToString(entry : BibTeXEntry, key : String = "XXX") : String = {
    val buffer = new StringBuilder
    buffer.append("@" + entry.entryType + "{" + key + ",\n")

    def printOptField[T](name : String, value : BibTeXEntry=>Option[T]) {
      value(entry).foreach(content => {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(latexify(content.toString))
        buffer.append("},\n")
      })
    }

    def printSeqField[T](name : String, values : BibTeXEntry=>Seq[T]) {
      val content = values(entry)
      if(!content.isEmpty) {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content.map(c => latexify(c.toString)).mkString(" and "))
        buffer.append("},\n")
      }
    }

    printSeqField("author", _.authors)
    printSeqField("editor", _.editors)
    printOptField("title", _.title)
    printOptField("booktitle", _.booktitle)
    printOptField("journal", _.journal)
    printOptField("pages", _.pages)
    printOptField("chapter", _.chapter)
    printOptField("volume", _.volume)
    printOptField("number", _.number)
    printOptField("series", _.series)
    printOptField("month", _.month)
    printOptField("year", _.year)
    printOptField("address", _.address)
    printOptField("edition", _.edition)
    printOptField("institution", _.institution)
    printOptField("howpublished", _.howpublished)
    printOptField("key", _.key)
    printOptField("organization", _.organization)
    printOptField("publisher", _.publisher)
    printOptField("school", _.school)
    printOptField("type", _.trType)
    printOptField("url", _.url)
    printOptField("eprint", _.eprint)
    printOptField("annote", _.annote)
    printOptField("note", _.note)

    buffer.dropRight(2).append("\n}").toString
  }

  private def acceptableASCII(c : Char) : Boolean = {
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  }
  private def accentRemove(c : Char) : Seq[Char] = c match {
    case 'À' | 'Á' | 'Â' | 'Ã' => "A"
    case 'Ä' | 'Æ' => "AE"
    case 'Å' => "AA"
    case 'Ç' => "C"
    case 'È' | 'É' | 'Ê' | 'Ë' => "E"
    case 'Ì' | 'Í' | 'Î' | 'Ï' => "I"
    case 'Ð' => "D"
    case 'Ñ' => "N"
    case 'Ò' | 'Ó' | 'Ô' | 'Õ' => "O"
    case 'Ö' | 'Ø' => "OE"
    case 'Ù' | 'Ú' | 'Û' => "U"
    case 'Ü' => "UE"
    case 'Ý' => "Y"
    case 'Þ' => "TH"
    case 'ß' => "ss"
    case 'à' | 'á' | 'â' | 'ã' => "a"
    case 'ä' | 'æ' => "ae"
    case 'å' => "aa"
    case 'ç' => "c"
    case 'è' | 'é' | 'ê' | 'ë' => "e"
    case 'ì' | 'í' | 'î' | 'ï' => "i"
    case 'ð' => "d"
    case 'ñ' => "n"
    case 'ò' | 'ó' | 'ô' | 'õ' => "o"
    case 'ö' | 'ø' => "oe"
    case 'ù' | 'ú' | 'û' => "u"
    case 'ü' => "ue"
    case 'ý' | 'ÿ' => "y"
    case 'þ' => "th"
    case x if acceptableASCII(x) => Seq(x)
    case other => ""
  }

  private def latexify(str : String) : String = str.flatMap(substForLaTeX)
  private def substForLaTeX(c : Char) : Seq[Char] = c match {
    case 'ø' => """{\o}"""
    case x => Seq(x)
  }
}
