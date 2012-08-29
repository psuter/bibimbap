package bibimbap
package util

import java.io.{File,FileInputStream}
import java.security.MessageDigest

object FileUtils {
  def md5(file : File) : Option[String] = try {
    val fis =  new FileInputStream(file);
    val bytes : Array[Byte] = new Array[Byte](1024)
    val digest = MessageDigest.getInstance("MD5")
    var read : Int = 0

    do {
      read = fis.read(bytes)
      if(read > 0) {
        digest.update(bytes, 0, read)
      }
    } while (read != -1)

    fis.close()

    val sb = new StringBuilder()
    for(b <- digest.digest()) {
      sb.append(Integer.toHexString((b & 0xFF) | 0x100).substring(1,3))
    }
    Some(sb.toString)
  } catch {
    case _ : Throwable => None
  }
}
