package bibimbap
package cache

import bibimbap.data._

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.queryParser.ParseException
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Query
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.TopScoreDocCollector
import org.apache.lucene.store.Directory
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.util.Version

class CacheModule(settings : Settings) extends SearchModule(settings) {
  module =>

  val name = "Local cache based on Lucene"

  val keyword = "cache"

  val dataSourceName = "Local Cache"

  override val moreActions  = Seq(searchAction)

  private val info : Any=>Unit = settings.logger.info
  private val warn : Any=>Unit = settings.logger.warn

  private val analyzer = new StandardAnalyzer(Version.LUCENE_36)
  private val index    = new RAMDirectory()
  private val config   = new IndexWriterConfig(Version.LUCENE_36, analyzer)
  private val writer   = new IndexWriter(index, config)
  writer.commit()

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
      doc.add(new Field("title", title, Field.Store.YES, Field.Index.ANALYZED))
    }

    if(!entry.authors.isEmpty) {
      doc.add(new Field("authors", entry.authors.mkString(" "), Field.Store.YES, Field.Index.ANALYZED))
    }

    for(url <- link) {
      doc.add(new Field("url", url, Field.Store.YES, Field.Index.NO))
    }

    val sb = new StringBuilder()
    entry.title.foreach(sb.append(_))
    sb.append(" ")
    entry.authors.foreach { author =>
      sb.append(author)
      sb.append(" ")
    }
    entry.journal.foreach(sb.append(_))
    entry.booktitle.foreach(sb.append(_))
    entry.year.foreach(sb.append(_))
    
    doc.add(new Field("blob", sb.toString, Field.Store.NO, Field.Index.ANALYZED))

    val bytes : Array[Byte] = entry.serialized
    doc.add(new Field("serialized", bytes))

    writer.addDocument(doc)
    writer.commit()
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
}
