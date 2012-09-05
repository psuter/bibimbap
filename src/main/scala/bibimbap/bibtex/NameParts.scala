package bibimbap
package bibtex

class NameParts(
  val first : Option[String],
  val von : Option[String],
  val last : Option[String],
  val junior : Option[String])

object NameParts {
  def unapply(np : NameParts) : Option[(Option[String],Option[String],Option[String],Option[String])] = if(np == null) None else {
    Some((np.first, np.von, np.last, np.junior))
  }

  def fromString(str : String) : NameParts = {
    def s2o(strs : Seq[String]) : Option[String] = {
      if(strs.isEmpty) {
        None
      } else {
        Some(strs.mkString(" "))
      }
    }

    def vonAndLast(parts : Seq[String], last : String) : (Seq[String], Seq[String]) = {
      val (lstR, vonR) = parts.reverse.span(part => !startsWithLowercase(part))
      (vonR.reverse, (last +: lstR).reverse)
    }

    // The final filter makes it behave differently than BibTeX is
    // pathological cases where you have a comma and nothing after/before it.
    val bits = str.split(',').map(_.trim).filter(!_.isEmpty)
    bits.length match {
      case 1 => {
        val parts = toParts(bits(0))
        val last  = parts.last
        val (fst, rest) = parts.dropRight(1).span(part => !startsWithLowercase(part))
        val (von, lst) = vonAndLast(rest, last)
        new NameParts(s2o(fst), s2o(von), s2o(lst), None)
      }

      case 2 => {
        val parts0 = toParts(bits(0))
        val (von, lst) = vonAndLast(parts0.dropRight(1), parts0.last)
        val fst = toParts(bits(1))
        new NameParts(s2o(fst), s2o(von), s2o(lst), None)
      }

      case x if x > 2 => {
        val parts0 = toParts(bits(0))
        val (von, lst) = vonAndLast(parts0.dropRight(1), parts0.last)
        val jr = toParts(bits(1))
        val fst = toParts(bits(2))
        new NameParts(s2o(fst), s2o(von), s2o(lst), s2o(jr))
      }

      // If more than 3, we drop the later groups.
      // No idea what BibTeX does/should do.
    }
  }

  private def toParts(str : String) : Seq[String] = {
    // TODO make sure {LaTeX groups} are kept as one part.
    str.split(' ').filter(!_.isEmpty)
  }

  private def startsWithLowercase(str : String) : Boolean = {
    // TODO skip LaTeX macros, and/or curly braces..
    !str.isEmpty && Character.isLowerCase(str(0))
  }
}
