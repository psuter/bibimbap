package bibimbap
package data

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
