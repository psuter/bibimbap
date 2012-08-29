package bibimbap
package util

import org.apache.commons.lang3.{StringUtils=>ApacheStringUtils}
import org.apache.commons.lang3.StringEscapeUtils

object StringUtils {
  def unescapeHTML(str : String) : String = {
    StringEscapeUtils.unescapeHtml4(
      StringEscapeUtils.unescapeHtml4(str)
    )
  }

  def trimRight(str : String) : String = {
    // `null` means strip whitespace... Go figure.
    ApacheStringUtils.stripEnd(str, null) 
  }

  def trimLeft(str : String) : String = {
    ApacheStringUtils.stripStart(str, null)
  }

  // Trim and replace any sequence of whitespaces by a single one.
  def normalizeSpace(str : String) : String = {
    ApacheStringUtils.normalizeSpace(str)
  }
}
