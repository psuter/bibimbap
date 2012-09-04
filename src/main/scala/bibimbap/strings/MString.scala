package bibimbap
package strings

///** A wrapper around strings to support converting to and from different formats. */
//final class MString private[strings](val toJava : String) {
//  def toLaTeX : String = MString.javaToLaTeX(toJava)
//  def toASCII : String = MString.javaToASCII(toJava)
//
//  override def toString : String = toJava
//
//  final def isEmpty : Boolean = toJava.isEmpty
//}

case class MString(latex: Option[String] = None, java: Option[String] = None, ascii: Option[String] = None) {
  lazy val toLaTeX: String = latex.orElse(java.map(MString.javaToLaTeX)).orElse(ascii.map(MString.asciiToLaTeX)).get
  lazy val toJava:  String = java.orElse(latex.map(MString.latexToJava)).orElse(ascii.map(MString.asciiToJava)).get
  lazy val toASCII: String = ascii.orElse(java.map(MString.javaToASCII)).orElse(latex.map(MString.latexToASCII)).get

  override def toString = { throw new Exception("Don't use MString.toString, use toJava/toLaTeX/toASCII") }
  
  assert(!latex.isEmpty || !java.isEmpty || !ascii.isEmpty)
}

object MString {
  def fromJava(str : String)  = new MString(java = Some(str))
  def fromLaTeX(str : String) = new MString(latex = Some(str))
  def fromASCII(str : String) = new MString(ascii = Some(str))

  val mapJavaToLatex = Map[Char, Seq[Char]](
    '#' -> """{\#}""",
    'ç' -> """\c{c}""",
    'Ç' -> """\c{C}""",
    'á' -> """\'{a}""",
    'Á' -> """\'{A}""",
    'é' -> """\'{e}""",
    'É' -> """\'{E}""",
    'í' -> """\'{\i}""",
    'Í' -> """\'{I}""",
    'ó' -> """\'{o}""",
    'Ó' -> """\'{O}""",
    'ú' -> """\'{u}""",
    'Ú' -> """\'{U}""",
    'ý' -> """\'{y}""",
    'Ý' -> """\'{Y}""",
    'à' -> """\`{a}""",
    'À' -> """\`{A}""",
    'è' -> """\`{e}""",
    'È' -> """\`{E}""",
    'ì' -> """\`{\i}""",
    'Ì' -> """\`{I}""",
    'ò' -> """\`{o}""",
    'Ò' -> """\`{O}""",
    'ù' -> """\`{u}""",
    'Ù' -> """\`{U}""",
    'ỳ' -> """\`{y}""",
    'Ỳ' -> """\`{Y}""",
    'â' -> """\^{a}""",
    'Â' -> """\^{A}""",
    'ê' -> """\^{e}""",
    'Ê' -> """\^{E}""",
    'î' -> """\^{\i}""",
    'Î' -> """\^{I}""",
    'ô' -> """\^{o}""",
    'Ô' -> """\^{O}""",
    'û' -> """\^{u}""",
    'Û' -> """\^{U}""",
    'ŷ' -> """\^{y}""",
    'Ŷ' -> """\^{Y}""",
    'æ' -> """{\ae}""",
    'Æ' -> """{\AE}""",
    'å' -> """{\aa}""",
    'Å' -> """{\AA}""",
    'œ' -> """{\oe}""",
    'Œ' -> """{\OE}""",
    'ø' -> """{\o}""",
    'Ø' -> """{\O}""",
    'ä' -> """\"{a}""",
    'Ä' -> """\"{A}""",
    'ë' -> """\"{e}""",
    'Ë' -> """\"{E}""",
    'ï' -> """\"{i}""",
    'Ï' -> """\"{I}""",
    'ö' -> """\"{o}""",
    'Ö' -> """\"{O}""",
    'ü' -> """\"{u}""",
    'Ü' -> """\"{U}""",
    'ÿ' -> """\"{y}""",
    'Ÿ' -> """\"{Y}""",
    'α' -> """$\alpha$""",
    'β' -> """$\beta$""",
    'γ' -> """$\gamma$""",
    'δ' -> """$\delta$""",
    'ε' -> """$\varepsilon$""",
    'ϵ' -> """$\epsilon$""",
    'ζ' -> """$\zeta$""",
    'η' -> """$\eta$""",
    'θ' -> """$\theta$""",
    'ι' -> """$\iota$""",
    'κ' -> """$\kappa$""",
    'λ' -> """$\lambda$""",
    'μ' -> """$\mu$""",
    'ν' -> """$\nu$""",
    'ξ' -> """$\xi$""",
    'ο' -> """$\omicron$""",
    'π' -> """$\pi$""",
    'ρ' -> """$\rho$""",
    'ς' -> """$\varsigma$""",
    'σ' -> """$\sigma$""",
    'τ' -> """$\tau$""",
    'υ' -> "$\\upsilon$",
    'φ' -> """$\phi$""",
    'χ' -> """$\chi$""",
    'ψ' -> """$\psi$""",
    'ω' -> """$\omega$"""
  )

  lazy val mapLatexToJava = mapJavaToLatex.map{ case (to, from) => from.mkString -> to.toString }

  def javaToLaTeX(str: String): String = {
    str.flatMap(mapJavaToLatex.orElse({ case x => Seq(x) }))
  }

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

      Normalizer.normalize(s, NFD).replaceAll("""\p{InCombiningDiacriticalMarks}+""", "")
    }

    def isASCII(c : Char) : Boolean = (c >= ' ' && c <= '~')  

    removeDiacritics(str.flatMap(transliterate)).filter(isASCII).trim
  }

  def latexToJava(str: String): String = {
    import java.util.regex.Pattern

    var res = str
    for ((from, to) <- mapLatexToJava) {
      res = res.replaceAll(Pattern.quote(from), to)
    }
    res
  }

  def asciiToJava(str: String): String = str

  def latexToASCII(str: String): String = (latexToJava _ andThen javaToASCII)(str)

  def asciiToLaTeX(str: String): String = (asciiToJava _ andThen javaToLaTeX)(str)
}
