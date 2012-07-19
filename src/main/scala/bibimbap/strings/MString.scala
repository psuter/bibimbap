package bibimbap
package strings

/** A wrapper around strings to support converting to and from different formats. */
final class MString(val toJava : String) extends Serializable {
  def toLaTeX : String = MString.javaToLaTeX(toJava)
  def toASCII : String = MString.javaToASCII(toJava)

  override def toString : String = toJava
}

object MString {
  def fromJava(str : String) = new MString(str)

  def javaToLaTeX(str : String) = {
    def substEntity(c : Char) : Seq[Char] = c match {
      case 'ø' => """{\o}"""
      case x => Seq(x)
    }

    str.flatMap(substEntity)
  }

  // To give the prettiest result, this function works in three phases.
  //   1) it substitutes common European diacritics with expanded form
  //   2) it strips all remaining diacritics using Unicode normalization
  //   3) it removes all non-ascii characters.
  def javaToASCII(str : String) = {
    def rewriteDiacritic(c : Char) : Seq[Char] = c match {
      case 'Ä' | 'Æ' => "AE"
      case 'Å'       => "AA"
      case 'Ö' | 'Ø' => "OE"
      case 'Ü'       => "UE"
      case 'Þ'       => "TH"
      case 'ß'       => "ss"
      case 'ä' | 'æ' => "ae"
      case 'å'       => "aa"
      case 'ö' | 'ø' => "oe"
      case 'ü'       => "ue"
      case 'þ'       => "th"
      case _         => Seq(c)
    }

    def removeDiacritics(s : String) : String = {
      import java.text.Normalizer
      import Normalizer.Form.NFD

      Normalizer.normalize(s, NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    }

    def isASCII(c : Char) : Boolean = (c >= ' ' && c <= '~')  

    removeDiacritics(str.flatMap(rewriteDiacritic)).filter(isASCII)
  }
}
