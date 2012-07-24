package bibimbap
package strings

/** A wrapper around strings to support converting to and from different formats. */
final class MString private[strings](val toJava : String) {
  def toLaTeX : String = MString.javaToLaTeX(toJava)
  def toASCII : String = MString.javaToASCII(toJava)

  override def toString : String = toJava

  final def toIntOpt : Option[Int] = try {
    Some(toJava.toInt)
  } catch {
    case nfe : NumberFormatException => None
  }

  final def isEmpty : Boolean = toJava.isEmpty
}

object MString {
  def fromJava(str : String) = new MString(str)

  def javaToLaTeX(str : String) = {
    def substEntity(c : Char) : Seq[Char] = c match {
      case 'ç' => """\c{c}"""
      case 'ø' => """{\o}"""
      case x => Seq(x)
    }

    str.flatMap(substEntity)
  }

  // To give the prettiest result, this function works in three phases.
  //   1) substitute common European diacritics and other letters with romanized form
  //   2) strip all remaining diacritics using Unicode normalization
  //   3) remove all non-ascii characters.
  def javaToASCII(str : String) = {
    def transliterate(c : Char) : Seq[Char] = c match {
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
      case 'α'       => "alpha"
      case 'β'       => "beta"
      case 'γ'       => "gamma"
      case 'δ'       => "delta"
      case 'ε' | 'ϵ' => "epsilon"
      case 'ζ'       => "zeta"
      case 'η'       => "eta"
      case 'θ'       => "theta"
      case 'ι'       => "iota"
      case 'κ'       => "kappa"
      case 'λ'       => "lambda"
      case 'μ'       => "mu"
      case 'ν'       => "nu"
      case 'ξ'       => "xi"
      case 'ο'       => "omicron"
      case 'π'       => "pi"
      case 'ρ'       => "rho"
      case 'ς' | 'σ' => "sigma"
      case 'τ'       => "tau"
      case 'υ'       => "upsilon"
      case 'φ'       => "phi"
      case 'χ'       => "chi"
      case 'ψ'       => "psi"
      case 'ω'       => "omega"
      case _         => Seq(c)
    }

    def removeDiacritics(s : String) : String = {
      import java.text.Normalizer
      import Normalizer.Form.NFD

      Normalizer.normalize(s, NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    }

    def isASCII(c : Char) : Boolean = (c >= ' ' && c <= '~')  

    removeDiacritics(str.flatMap(transliterate)).filter(isASCII)
  }
}
