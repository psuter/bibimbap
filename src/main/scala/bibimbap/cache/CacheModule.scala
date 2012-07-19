package bibimbap
package cache

import bibimbap.data._

import java.io.File

import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryParser._
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.Version

class CacheModule(settings : Settings) extends SearchModule(settings) {
  module =>

  val name = "Local cache based on Lucene"

  val keyword = "cache"

  val dataSourceName = "Local Cache"

  override val moreActions  = Seq(searchAction, clearAction)

  private val info : Any=>Unit = settings.logger.info
  private val warn : Any=>Unit = settings.logger.warn

  private val cacheDir = new File(settings("general", "dir.cache"))

  private val analyzer = new StandardAnalyzer(Version.LUCENE_36)

  private var index = initializeIndex()

  def initializeIndex() = {
    val idx = FSDirectory.open(cacheDir)
    // This should force the creation of the index if it didn't exist.
    val cfg = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    val w = new IndexWriter(idx, cfg)
    w.close()
    idx
  }

  override def onImport(sre : SearchResultEntry) {
    if(sre.source != module.keyword)
      addEntry(sre.entry, sre.link)
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
    
    for(title <- entry.title) {
      doc.add(new Field("title", title.toJava, Field.Store.YES, Field.Index.ANALYZED))
    }

    if(!entry.authors.isEmpty) {
      doc.add(new Field("authors", entry.authors.map(_.toJava).mkString(" "), Field.Store.YES, Field.Index.ANALYZED))
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

    val bytes : Array[Byte] = entry.serialized
    doc.add(new Field("serialized", bytes))

    val config = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    val writer = new IndexWriter(index, config)
    writer.addDocument(doc)
    writer.close()
  }

  private def documentToSearchResultEntry(document : Document) : Option[SearchResultEntry] = {
    for(bytes <- Option(document.getBinaryValue("serialized"));
        entry <- BibTeXEntry.deserialize(bytes)) yield {
      val url = Option(document.get("url"))
      SearchResultEntry(entry, () => entry, url, module.keyword) 
    }
  }

  lazy val searchAction = new Action[SearchResult]("search") {
    val description = "Search for records in the local cache."

    def run(args : String*) : SearchResult = {
      val query = args.mkString(" ").trim
      if(query.isEmpty) {
        Nil
      } else {
        searchEntries(query).flatMap(documentToSearchResultEntry)
      }
    }
  }

  lazy val clearAction = new Action[Unit]("clear") {
    val description = "Clear the local cache."

    def run(args : String*) : Unit = {
      import org.apache.commons.io.FileUtils
      import java.io.IOException
      try {
        FileUtils.deleteDirectory(cacheDir)
        index = initializeIndex()
      } catch {
        case ioe : IOException => warn(ioe.getLocalizedMessage)
      }
    } 
  }
}
