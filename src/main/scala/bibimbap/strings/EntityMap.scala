// This class may become useful again if we ever have a "fromHTML" method for MStrings
/*
package bibimbap
package dblp

// This list of entities was partially recovered from dblp.dtd, the rest added
// manually.
object EntityMap {
  def apply(entity : String) : String = entity match {
    case "amp"      => "&" 
    case "quot"     => "\""
    case "apos"     => "'"
    case "lt"       => "<"
    case "gt"       => ">"
    // Following is from dblp.dtd.
    case "reg"      => "®" // "&#174;" registered trademark
    case "micro"    => "µ" // "&#181;" micro
    case "times"    => "×" // "&#215;" times, multiplication
    case "Agrave"   => "À" // "&#192;" capital A, grave accent 
    case "Aacute"   => "Á" // "&#193;" capital A, acute accent 
    case "Acirc"    => "Â" // "&#194;" capital A, circumflex accent 
    case "Atilde"   => "Ã" // "&#195;" capital A, tilde 
    case "Auml"     => "Ä" // "&#196;" capital A, dieresis or umlaut mark 
    case "Aring"    => "Å" // "&#197;" capital A, ring 
    case "AElig"    => "Æ" // "&#198;" capital AE diphthong (ligature) 
    case "Ccedil"   => "Ç" // "&#199;" capital C, cedilla 
    case "Egrave"   => "È" // "&#200;" capital E, grave accent 
    case "Eacute"   => "É" // "&#201;" capital E, acute accent 
    case "Ecirc"    => "Ê" // "&#202;" capital E, circumflex accent 
    case "Euml"     => "Ë" // "&#203;" capital E, dieresis or umlaut mark 
    case "Igrave"   => "Ì" // "&#204;" capital I, grave accent 
    case "Iacute"   => "Í" // "&#205;" capital I, acute accent 
    case "Icirc"    => "Î" // "&#206;" capital I, circumflex accent 
    case "Iuml"     => "Ï" // "&#207;" capital I, dieresis or umlaut mark 
    case "ETH"      => "Ð" // "&#208;" capital Eth, Icelandic 
    case "Ntilde"   => "Ñ" // "&#209;" capital N, tilde 
    case "Ograve"   => "Ò" // "&#210;" capital O, grave accent 
    case "Oacute"   => "Ó" // "&#211;" capital O, acute accent 
    case "Ocirc"    => "Ô" // "&#212;" capital O, circumflex accent 
    case "Otilde"   => "Õ" // "&#213;" capital O, tilde 
    case "Ouml"     => "Ö" // "&#214;" capital O, dieresis or umlaut mark 
    case "Oslash"   => "Ø" // "&#216;" capital O, slash 
    case "Ugrave"   => "Ù" // "&#217;" capital U, grave accent 
    case "Uacute"   => "Ú" // "&#218;" capital U, acute accent 
    case "Ucirc"    => "Û" // "&#219;" capital U, circumflex accent 
    case "Uuml"     => "Ü" // "&#220;" capital U, dieresis or umlaut mark 
    case "Yacute"   => "Ý" // "&#221;" capital Y, acute accent 
    case "Yuml"     => "Ÿ"
    case "THORN"    => "Þ" // "&#222;" capital THORN, Icelandic 
    case "szlig"    => "ß" // "&#223;" small sharp s, German (sz ligature) 
    case "agrave"   => "à" // "&#224;" small a, grave accent    
    case "aacute"   => "á" // "&#225;" small a, acute accent 
    case "acirc"    => "â" // "&#226;" small a, circumflex accent 
    case "atilde"   => "ã" // "&#227;" small a, tilde 
    case "auml"     => "ä" // "&#228;" small a, dieresis or umlaut mark 
    case "aring"    => "å" // "&#229;" small a, ring 
    case "aelig"    => "æ" // "&#230;" small ae diphthong (ligature) 
    case "ccedil"   => "ç" // "&#231;" small c, cedilla 
    case "egrave"   => "è" // "&#232;" small e, grave accent 
    case "eacute"   => "é" // "&#233;" small e, acute accent 
    case "ecirc"    => "ê" // "&#234;" small e, circumflex accent 
    case "euml"     => "ë" // "&#235;" small e, dieresis or umlaut mark 
    case "igrave"   => "ì" // "&#236;" small i, grave accent 
    case "iacute"   => "í" // "&#237;" small i, acute accent 
    case "icirc"    => "î" // "&#238;" small i, circumflex accent 
    case "iuml"     => "ï" // "&#239;" small i, dieresis or umlaut mark 
    case "eth"      => "ð" // "&#240;" small eth, Icelandic 
    case "ntilde"   => "ñ" // "&#241;" small n, tilde 
    case "ograve"   => "ò" // "&#242;" small o, grave accent 
    case "oacute"   => "ó" // "&#243;" small o, acute accent 
    case "ocirc"    => "ô" // "&#244;" small o, circumflex accent 
    case "otilde"   => "õ" // "&#245;" small o, tilde 
    case "ouml"     => "ö" // "&#246;" small o, dieresis or umlaut mark 
    case "oslash"   => "ø" // "&#248;" small o, slash 
    case "ugrave"   => "ù" // "&#249;" small u, grave accent 
    case "uacute"   => "ú" // "&#250;" small u, acute accent 
    case "ucirc"    => "û" // "&#251;" small u, circumflex accent 
    case "uuml"     => "ü" // "&#252;" small u, dieresis or umlaut mark 
    case "yacute"   => "ý" // "&#253;" small y, acute accent 
    case "yuml"     => "ÿ" // "&#255;" small y, dieresis or umlaut mark   
    case "thorn"    => "þ" // "&#254;" small thorn, Icelandic 
    case _          => throw new Exception("Unknown entity : " + entity)
  }
}
*/
