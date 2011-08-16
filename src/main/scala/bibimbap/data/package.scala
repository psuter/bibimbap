package bibimbap

package object data {
  // The idea of the pair of entry, ()=>entry is that in some cases it is
  // cheaper to get an incomplete version of the entry which is still
  // sufficient for the purpose of displaying search results. The callback is
  // used to "precise" the entry once it is examined or imported.
  case class SearchResultEntry(entry : BibTeXEntry, callback : ()=>BibTeXEntry, link : Option[String])
  type SearchResult = Iterable[SearchResultEntry]

  object BibTeXEntryTypes extends Enumeration {
    type BibTeXEntryType = Value
    val Article = Value("article")
    val Book = Value("book")
    val Booklet = Value("booklet")
    val InBook = Value("inbook")
    val InCollection = Value("incollection")
    val InProceedings = Value("inproceedings")
    val Manual = Value("manual")
    val MastersThesis = Value("mastersthesis")
    val Misc = Value("misc")
    val PhDThesis = Value("phdthesis")
    val Proceedings = Value("proceedings")
    val TechReport = Value("techreport")
    val Unpublished = Value("unpublished")
  }

  // This datatypes and all the following ones assume crossrefs have been
  // "resolved" into all entries.
  trait BibTeXEntry {
    val entryType : BibTeXEntryTypes.BibTeXEntryType

    val address      : Option[String] = None
    val annote       : Option[String] = None
    val authors      : Seq[String]    = Seq.empty
    val booktitle    : Option[String] = None
    val chapter      : Option[Int]    = None
    val edition      : Option[String] = None
    val editors      : Seq[String]    = Seq.empty
    val eprint       : Option[String] = None
    val howpublished : Option[String] = None
    val institution  : Option[String] = None
    val journal      : Option[String] = None
    val key          : Option[String] = None
    val month        : Option[String] = None
    val note         : Option[String] = None
    val number       : Option[String] = None
    val organization : Option[String] = None
    val pages        : Option[String] = None
    val publisher    : Option[String] = None
    val school       : Option[String] = None
    val series       : Option[String] = None
    val title        : Option[String] = None
    val trType       : Option[String] = None
    val url          : Option[String] = None
    val volume       : Option[String] = None
    val year         : Option[Int]    = None

    override def toString = entryToString(this, entryToKey(this))

    def inlineString = entryToInline(this)
  }

  final class InconsistentBibTeXEntry(msg : String) extends Exception(msg)
  private def forConsistency(msg : String)(expr : =>Boolean)(implicit entryType : BibTeXEntryTypes.BibTeXEntryType) {
    if(!expr) {
      throw new InconsistentBibTeXEntry("Error in " + entryType + ": " + msg + ".")
    }
  }

  final class Article(
    auth : Seq[String], titl : String, jour : String, yr : Int,
    override val volume : Option[String] = None,
    override val number : Option[String] = None,
    override val pages : Option[String] = None,
    override val month : Option[String] = None,
    override val note : Option[String] = None,
    override val key : Option[String] = None
  ) extends BibTeXEntry {
    implicit val entryType = BibTeXEntryTypes.Article

    forConsistency("author list must be defined") {
      !auth.isEmpty
    }

    override val authors = auth
    override val title   = Some(titl)
    override val journal = Some(jour)
    override val year    = Some(yr) 
  }

  final class Book(
    auth : Seq[String], editrs : Seq[String], titl : String, pubshr : String, yr : Int,
    override val volume : Option[String] = None,
    override val series : Option[String] = None,
    override val address : Option[String] = None,
    override val edition : Option[String] = None,
    override val month : Option[String] = None,
    override val note : Option[String] = None,
    override val key : Option[String] = None
  ) extends BibTeXEntry {
    implicit val entryType = BibTeXEntryTypes.Book

    forConsistency("author list or editor list must be defined") {
      !(auth.isEmpty && editrs.isEmpty)
    }

    override val authors   = auth
    override val editors   = editrs
    override val title     = Some(titl)
    override val publisher = Some(pubshr)
    override val year      = Some(yr)
  }

  // Missing : booklet, conference, inbook, incollection

  final class InProceedings(
    auth : Seq[String], titl : String, bktitl : String, yr : Int,
    override val editors : Seq[String] = Seq.empty,
    override val series : Option[String] = None,
    override val pages : Option[String] = None,
    override val organization : Option[String] = None,
    override val publisher : Option[String] = None,
    override val address : Option[String] = None,
    override val month : Option[String] = None,
    override val note : Option[String] = None,
    override val key : Option[String] = None
  ) extends BibTeXEntry {
    implicit val entryType = BibTeXEntryTypes.InProceedings

    forConsistency("author list must be defined") {
      !auth.isEmpty
    }

    override val authors   = auth
    override val title     = Some(titl)
    override val booktitle = Some(bktitl)
    override val year      = Some(yr)
  }

  // Missing : manual, mastersthesis, misc, phdthesis

  final class Proceedings(
    titl : String, yr : Int,
    override val editors : Seq[String] = Seq.empty,
    override val publisher  : Option[String] = None,
    override val organization : Option[String] = None,
    override val address : Option[String] = None,
    override val month : Option[String] = None,
    override val note : Option[String] = None,
    override val key : Option[String] = None
  ) extends BibTeXEntry {
    implicit val entryType = BibTeXEntryTypes.Proceedings

    override val title = Some(titl)
    override val year  = Some(yr)
  }

  // Missing : techreport, unpublished

  private def camelcasify(str : String) : Seq[String] = {
    val common = Set("", "in", "the", "a", "of", "for", "and", "or")
    str.split(" ")
      .map(_.toLowerCase)
      .filterNot(common(_))
      .map(_.flatMap(accentRemove(_)))
      .map(_.capitalize)
  }

  private def entryToKey(entry : BibTeXEntry) : String = {
    val persons   = if(!entry.authors.isEmpty) entry.authors else entry.editors
    val lastnames = if(persons.size > 3) {
      persons(0).split(" ").last.flatMap(accentRemove(_)) + "ETAL"
    } else {
      persons.map(ss => ss.split(" ").last).mkString("").flatMap(accentRemove(_))
    }

    val yr = entry.year match {
      case Some(y) => {
        val last = y % 100
        if(last < 10) "0" + last else last.toString
      }
      case None => ""
    }
    val title = entry.title match {
      case Some(t) => camelcasify(t).take(6).mkString("").flatMap(accentRemove(_))
      case None => ""
    }

    lastnames + yr + title
  }

  // Tries to shorten the first names (whatever that means).
  private def shortenName(name : String) : String = {
    val elements = name.split(" ").filterNot(_.isEmpty)
    elements.dropRight(1).map(e => e(0) + ".").mkString("") + elements.last
  }

  private def entryToInline(entry : BibTeXEntry) : String = {
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

  private def entryToString(entry : BibTeXEntry, key : String = "XXX") : String = {
    val buffer = new StringBuilder
    buffer.append("@" + entry.entryType + "{" + key + ",\n")

    def printOptField[T](name : String, value : BibTeXEntry=>Option[T]) {
      value(entry).foreach(content => {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content.toString)
        buffer.append("},\n")
      })
    }

    def printSeqField[T](name : String, values : BibTeXEntry=>Seq[T]) {
      val content = values(entry)
      if(!content.isEmpty) {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content.mkString(" and "))
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
    case validLetter if validLetter >= 'A' && validLetter <= 'z' => Seq(validLetter)
    case other => ""
  }
}
