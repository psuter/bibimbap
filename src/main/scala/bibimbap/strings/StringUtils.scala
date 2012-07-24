package bibimbap
package strings

import org.apache.commons.lang3.StringEscapeUtils

object StringUtils {
  def unescapeHTML(str : String) : String = {
    StringEscapeUtils.unescapeHtml4(
      StringEscapeUtils.unescapeHtml4(str)
    )
  }
}
