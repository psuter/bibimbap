package bibimbap
package data

import bibimbap.strings._

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
trait BibTeXEntry extends Serializable {
  val entryType : BibTeXEntryTypes.BibTeXEntryType

  val address      : Option[MString] = None
  val annote       : Option[MString] = None
  val authors      : Seq[MString]    = Seq.empty
  val booktitle    : Option[MString] = None
  val chapter      : Option[Int]     = None
  val edition      : Option[MString] = None
  val editors      : Seq[MString]    = Seq.empty
  val eprint       : Option[MString] = None
  val howpublished : Option[MString] = None
  val institution  : Option[MString] = None
  val journal      : Option[MString] = None
  val key          : Option[MString] = None
  val month        : Option[MString] = None
  val note         : Option[MString] = None
  val number       : Option[MString] = None
  val organization : Option[MString] = None
  val pages        : Option[MString] = None
  val publisher    : Option[MString] = None
  val school       : Option[MString] = None
  val series       : Option[MString] = None
  val title        : Option[MString] = None
  val trType       : Option[MString] = None
  val url          : Option[MString] = None
  val volume       : Option[MString] = None
  val year         : Option[Int]     = None

  override def toString = entryToString(this, entryToKey(this))

  def getKey = entryToKey(this)

  def inlineString = entryToInline(this)

  lazy val entryMap : Map[String,MString] = {
    var base = Map("type" -> MString.fromJava(entryType.toString))
    def add(name : String, f : Option[MString]) {
      f.foreach(ms => base = base + (name -> ms))
    }
    def addSeq(name : String, seq : Seq[MString]) {
      if(!seq.isEmpty) {
        base = base + (name -> MString.fromJava(seq.map(_.toJava).mkString(" and ")))
      }
    }
    add("address",      this.address)
    add("annote",       this.annote)
    addSeq("authors",   this.authors)
    add("booktitle",    this.booktitle)
    add("chapter",      this.chapter.map(c => MString.fromJava(c.toString)))
    add("edition",      this.edition)
    addSeq("editors",   this.editors)
    add("eprint",       this.eprint)
    add("howpublished", this.howpublished)
    add("institution",  this.institution)
    add("journal",      this.journal)
    add("key",          this.key)
    add("month",        this.month)
    add("note",         this.note)
    add("number",       this.number)
    add("organization", this.organization)
    add("pages",        this.pages)
    add("publisher",    this.publisher)
    add("school",       this.school)
    add("series",       this.series)
    add("title",        this.title)
    add("trType",       this.trType)
    add("url",          this.url)
    add("volume",       this.volume)
    add("year",         this.year.map(y => MString.fromJava(y.toString)))

    base
  }
}

object BibTeXEntry {
  def fromEntryMap(map : Map[String,MString]) : Option[BibTeXEntry] = {
    def get(key : String) : Option[MString] = map.get(key)
    def getInt(key : String) : Option[Int] = map.get(key).flatMap(_.toIntOpt)
    def getSeq(key : String) : Seq[MString] = {
      map.get(key).map(ms => {
        val ss : Seq[MString] = ms.toJava.split(" and ").map(s => MString.fromJava(s))
        ss
      }).getOrElse(
        Seq.empty[MString]
      )
    }

    map.get("type").flatMap(_.toJava match {
      case "article" => for(
        a <- Some(getSeq("authors")) if !a.isEmpty;
        t <- get("title");
        j <- get("journal");
        y <- getInt("year")) yield {
        new Article(a, t, j, y,
          volume = get("volume"),
          number = get("number"),
          pages = get("pages"),
          month = get("month"),
          note = get("note"),
          key = get("key")
        )
      }

      case "inproceedings" => for(
        a <- Some(getSeq("authors")) if !a.isEmpty;
        t <- get("title");
        b <- get("booktitle");
        y <- getInt("year")) yield {
        new InProceedings(a, t, b, y,
          editors = getSeq("editors"),
          series = get("series"),
          pages = get("pages"),
          organization = get("organization"),
          publisher = get("publisher"),
          address = get("address"),
          month = get("month"),
          note = get("note"),
          key = get("key")
        ) 
      }

      case _ => None
    })
  }
}

final class InconsistentBibTeXEntry(msg : String) extends Exception(msg)

final class Article(
  auth : Seq[MString], titl : MString, jour : MString, yr : Int,
  override val volume : Option[MString] = None,
  override val number : Option[MString] = None,
  override val pages : Option[MString] = None,
  override val month : Option[MString] = None,
  override val note : Option[MString] = None,
  override val key : Option[MString] = None
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
  auth : Seq[MString], editrs : Seq[MString], titl : MString, pubshr : MString, yr : Int,
  override val volume : Option[MString] = None,
  override val series : Option[MString] = None,
  override val address : Option[MString] = None,
  override val edition : Option[MString] = None,
  override val month : Option[MString] = None,
  override val note : Option[MString] = None,
  override val key : Option[MString] = None
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
  auth : Seq[MString], titl : MString, bktitl : MString, yr : Int,
  override val editors : Seq[MString] = Seq.empty,
  override val series : Option[MString] = None,
  override val pages : Option[MString] = None,
  override val organization : Option[MString] = None,
  override val publisher : Option[MString] = None,
  override val address : Option[MString] = None,
  override val month : Option[MString] = None,
  override val note : Option[MString] = None,
  override val key : Option[MString] = None
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
  titl : MString, yr : Int,
  override val editors : Seq[MString] = Seq.empty,
  override val publisher  : Option[MString] = None,
  override val organization : Option[MString] = None,
  override val address : Option[MString] = None,
  override val month : Option[MString] = None,
  override val note : Option[MString] = None,
  override val key : Option[MString] = None
) extends BibTeXEntry {
  implicit val entryType = BibTeXEntryTypes.Proceedings

  override val title = Some(titl)
  override val year  = Some(yr)
}

// Missing : techreport, unpublished
