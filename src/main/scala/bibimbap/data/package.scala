package bibimbap

import strings._

package object data {
  case class SearchResult(entry : BibTeXEntry, link : Option[String], source : String)

  private[data] def forConsistency(msg : String)(expr : =>Boolean)(implicit entryType : BibTeXEntryTypes.BibTeXEntryType) {
    if(!expr) {
      throw new InconsistentBibTeXEntry("Error in " + entryType + ": " + msg + ".")
    }
  }

  private val commonWords = Set("", "in", "the", "a", "an", "of", "for", "and", "or", "by", "on", "with")
  private[data] def entryToKey(entry : BibTeXEntry) : String = {
    def isBibTeXFriendly(c : Char) : Boolean = (
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a') && (c <= 'z') ||
      (c >= '0') && (c <= '9')
    )

    def camelcasify(str : MString) : Seq[String] = {
      str.toJava.split(" ")
        .map(bit => MString.javaToASCII(bit).filter(isBibTeXFriendly))
        .filterNot(_.isEmpty)
        .map(_.toLowerCase)
        .filterNot(commonWords)
        .map(_.capitalize)
    }

    def lastFromPerson(person : MString) : String = {
      val lastBit = MString.fromJava(person.toJava.split(" ").last)
      lastBit.toASCII.filter(isBibTeXFriendly)
    }

    val persons   = if(!entry.authors.isEmpty) entry.authors else entry.editors
    val lastnames = if(persons.size > 3) {
      lastFromPerson(persons(0)) + "ETAL"
    } else {
      persons.map(lastFromPerson).mkString("")
    }

    val yr = entry.year match {
      case Some(y) => {
        val last = y % 100
        if(last < 10) "0" + last else last.toString
      }
      case None => ""
    }
    val title = entry.title.map(t =>
      camelcasify(t).take(6).mkString("")
    ).getOrElse("")

    lastnames + yr + title
  }

  // Tries to shorten the first names (whatever that means).
  private def shortenName(name : String) : String = {
    val elements = name.split(" ").filterNot(_.isEmpty)
    elements.dropRight(1).map(e => e(0) + ".").mkString("") + elements.last
  }

  private[data] def entryToInline(entry : BibTeXEntry) : String = {
    val (persons,areEditors) = if(!entry.authors.isEmpty) {
      (entry.authors, false)
    } else {
      (entry.editors, true)
    }

    val personString = if(persons.size > 4) {
      shortenName(persons.head.toJava) + " et al."
    } else {
      persons.map(p => shortenName(p.toJava)).mkString(", ")
    }

    val names = if(areEditors) (personString + " ed.") else personString

    val title = "\"" + entry.title.map(_.toJava).getOrElse("?") + "\""

    val where = 
      entry.booktitle.map(_.toJava).getOrElse(
        entry.journal.map(_.toJava).getOrElse(
          entry.school.map(_.toJava).getOrElse(
            entry.howpublished.map(_.toJava).getOrElse("?"))))

    val year = entry.year.map(_.toString).getOrElse("?")

    names + ", " + title + ", " + where + ", " + year
  }

  private[data] def entryToString(entry : BibTeXEntry, key : String = "XXX") : String = {
    val buffer = new StringBuilder
    buffer.append("@" + entry.entryType + "{" + key + ",\n")

    def printOptField(name : String, value : BibTeXEntry=>Option[MString]) {
      value(entry).foreach(content => {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content.toLaTeX)
        buffer.append("},\n")
      })
    }

    def printOptInt(name : String, value : BibTeXEntry=>Option[Int]) {
      value(entry).foreach(content => {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content)
        buffer.append("},\n")
      })
    }

    def printSeqField(name : String, values : BibTeXEntry=>Seq[MString]) {
      val content = values(entry)
      if(!content.isEmpty) {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content.map(_.toLaTeX).mkString(" and "))
        buffer.append("},\n")
      }
    }

    printSeqField("author", _.authors)
    printSeqField("editor", _.editors)
    printOptField("title", _.title)
    printOptField("booktitle", _.booktitle)
    printOptField("journal", _.journal)
    printOptField("pages", _.pages)
    printOptInt("chapter", _.chapter)
    printOptField("volume", _.volume)
    printOptField("number", _.number)
    printOptField("series", _.series)
    printOptField("month", _.month)
    printOptInt("year", _.year)
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
}
