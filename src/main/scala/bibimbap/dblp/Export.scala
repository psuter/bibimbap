package bibimbap
package dblp

import bibimbap.data._

trait Export {
  self : Search =>

  import settings.logger.{info,warn}

  private val PageRegExp = """(\d+)-+(\d+)""".r
  private def rewritePages(str : Option[String]) : Option[String] = str.map(_ match {
    case PageRegExp(begin, end) => begin + "--" + end
    case elze => elze
  })

  private def rewriteTitle(str : Option[String]) : Option[String] = str.map(str =>
    if(str.endsWith(".")) {
      str.dropRight(1)
    } else {
      str
    }
  )

  private def allPersonsToSeq(str : Option[String]) : Seq[String] = str match {
    case Some(ap) => ap.split(";")
    case None => Seq.empty
  }

  def export(entry : Entry) : Option[SearchResultEntry] = {
    //warn("export called on " + entry.kind)
    entry.kind match {
    case "inproceedings" if(
      entry.allAuthors.isDefined &&
      entry.title.isDefined &&
      entry.booktitle.isDefined &&
      entry.year.isDefined
    ) => Some(SearchResultEntry(
        new InProceedings(
          allPersonsToSeq(entry.allAuthors),
          rewriteTitle(entry.title).get,
          entry.booktitle.get,
          entry.year.get,
          allPersonsToSeq(entry.allEditors),
          series = entry.series,
          pages = rewritePages(entry.pages), 
          publisher = entry.publisher,
          address = entry.address,
          month = entry.month,
          note = entry.note),
        ()=>preciseExport(entry.key),
        entry.ee))

    case "article" if(
      entry.allAuthors.isDefined &&
      entry.title.isDefined &&
      entry.journal.isDefined && entry.journal.get != "CoRR" &&
      entry.year.isDefined
    ) => Some(SearchResultEntry(
        new Article(
          allPersonsToSeq(entry.allAuthors),
          rewriteTitle(entry.title).get,
          entry.journal.get,
          entry.year.get,
          volume = entry.volume,
          number = entry.number,
          pages = rewritePages(entry.pages),
          month = entry.month,
          note = entry.note),
        ()=>preciseExport(entry.key),
        entry.ee))
    
    case _ => None
  }}

  def preciseExport(dblpkey : String) : BibTeXEntry = {
    export(fullEntry(dblpkey)).get.entry
  }
}
