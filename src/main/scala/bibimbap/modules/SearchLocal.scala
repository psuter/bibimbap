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

class SearchLocal(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends SearchModule {
  val name = "SearchLocal"

  val source = "cache"

  private val cacheDir = new File(settings("general", "dir.cache"))

  private val analyzer = new StandardAnalyzer(Version.LUCENE_36)

  private var index = initializeIndex()

  override def search(terms: List[String]): SearchResults = {
    val query = terms.mkString(" ").trim
    if(query.isEmpty) {
      SearchResults(Nil)
    } else {
      SearchResults(searchEntries(query).flatMap(documentToSearchResult).toList)
    }
  }

  def clear() = {
    import org.apache.commons.io.FileUtils
    import java.io.IOException
    try {
      FileUtils.deleteDirectory(cacheDir)
      index = initializeIndex()
    } catch {
      case ioe : IOException =>
        console ! Warning(ioe.getLocalizedMessage)
    }
  }

  def initializeIndex() = {
    val idx = FSDirectory.open(cacheDir)
    // This should force the creation of the index if it didn't exist.
    val cfg = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    val w = new IndexWriter(idx, cfg)
    w.close()
    idx
  }

  override def onImport(res: SearchResult) {
    addEntry(res.entry, res.link)
  }

  private def searchEntries(query : String) : Iterable[Document] = {
    val q = new QueryParser(Version.LUCENE_36, "blob", analyzer).parse(query)
    val hitsPerPage = 10
    val reader = IndexReader.open(index)
    val searcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(hitsPerPage, true)
    searcher.search(q, collector)
    val hits : Array[ScoreDoc] = collector.topDocs.scoreDocs
    val docs = hits.map(hit => searcher.doc(hit.doc))
    searcher.close()
    docs
  }

  private def addEntry(entry : BibTeXEntry, link : Option[String]) : Unit = {
    val doc = new Document()
    
    for((k,v) <- entry.entryMap) {
      doc.add(new Field(k, v.toJava, Field.Store.YES, Field.Index.NO))
    }

    for(url <- link) {
      doc.add(new Field("url", url, Field.Store.YES, Field.Index.NO))
    }

    val sb = new StringBuilder()
    entry.title.foreach(sb.append(_))
    sb.append(" ")
    entry.authors.foreach { author =>
      sb.append(author.toJava)
      sb.append(" ")
    }
    entry.journal.foreach(j => sb.append(j.toJava))
    entry.booktitle.foreach(b => sb.append(b.toJava))
    entry.year.foreach(sb.append(_))
    
    doc.add(new Field("blob", sb.toString, Field.Store.NO, Field.Index.ANALYZED))

    val config = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    val writer = new IndexWriter(index, config)
    writer.addDocument(doc)
    writer.close()
  }

  private def documentToSearchResult(document : Document) : Option[SearchResult] = {
    import scala.collection.JavaConversions._
    val em : Map[String,MString] = document.getFields().map(f =>
      (f.name -> MString.fromJava(f.stringValue))
    ).toMap

    for(entry <- BibTeXEntry.fromEntryMap(em, console ! Error(_))) yield {
      val url = Option(document.get("url"))
      SearchResult(entry, url, Set(source))
    }
  }

  //lazy val clearAction = new Action[Unit]("clear") {
  //  val description = "Clear the local cache."

  //  def run(args : String*) : Unit = {
  //  } 
  //}
}
