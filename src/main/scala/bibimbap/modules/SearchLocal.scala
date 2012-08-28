package bibimbap
package modules

import akka.actor._
import data._
import strings._

import java.io.File

import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryParser._
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.Version

class SearchLocal(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends SearchProvider with LuceneHDDBackend with LuceneSearchProvider {
  val name = "SearchLocal"

  val source = "cache"

  protected val cacheDir = new File(settings("general", "dir.cache"))

  override def onImport(res: SearchResult) {
    addEntry(res.entry, res.link)
  }
}
