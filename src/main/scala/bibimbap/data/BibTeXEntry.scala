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
  val year         : Option[Int]    = None

  override def toString = entryToString(this, entryToKey(this))

  def getKey = entryToKey(this)

  def inlineString = entryToInline(this)

  final def serialized = BibTeXEntry.serialize(this)
}

object BibTeXEntry {
  def serialize(entry : BibTeXEntry) : Array[Byte] = {
    import java.io.{ByteArrayOutputStream,ObjectOutputStream}
    val baos = new ByteArrayOutputStream(1024)
    val oos  = new ObjectOutputStream(baos)
    oos.writeObject(entry)
    baos.toByteArray
  }

  def deserialize(bytes : Array[Byte]) : Option[BibTeXEntry] = {
    import java.io.{ByteArrayInputStream,ObjectInputStream}
    val bais = new ByteArrayInputStream(bytes)
    val ois  = new ObjectInputStream(bais)
    ois.readObject() match {
      case be : BibTeXEntry => Some(be)
      case _ => None
    } 
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
