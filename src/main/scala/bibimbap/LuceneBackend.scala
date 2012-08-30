package bibimbap

import bibtex._
import strings._

import akka.actor.ActorRef

import java.io.File

import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryParser._
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.Version

trait LuceneBackend {

  val console: ActorRef
  val source: String

  private val analyzer = new StandardAnalyzer(Version.LUCENE_36)
  private val config   = new IndexWriterConfig(Version.LUCENE_36, analyzer)

  def getLuceneIndex : Directory

  protected var index: Directory    = null
  protected var writer: IndexWriter = null

  def initializeIndex() = {
    val idx = getLuceneIndex
    // This should force the creation of the index if it didn't exist.
    val cfg = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    index = idx
    writer = new IndexWriter(index, config)
    writer.commit()
  }

  def getEntryByKey(key: String): Option[BibTeXEntry] = None

  def deleteEntryByKey(key: String) {
    writer.deleteDocuments(new Term("__key", key))
    writer.commit()
  }

  def searchLucene(query: String): List[SearchResult] =
    searchEntries(query).flatMap{ case (doc, score) => documentToSearchResult(doc, score) }.toList

  def addEntry(entry: BibTeXEntry): Unit =
    addEntries(List(entry))
    
  def addEntries(entries : Iterable[BibTeXEntry]) : Unit = {
    for (entry <- entries) {
      val doc = new Document()

      for((k,v) <- entry.entryMap) {
        doc.add(new Field(k, v.toJava, Field.Store.YES, Field.Index.NO))
      }

      doc.add(new Field("__key",  entry.key.getOrElse(""), Field.Store.YES, Field.Index.ANALYZED))
      doc.add(new Field("__type", entry.tpe.toString, Field.Store.YES, Field.Index.NO))

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

      doc.add(new Field("__blob", sb.toString, Field.Store.NO, Field.Index.ANALYZED))

      writer.addDocument(doc)
    }

    writer.commit()
  }

  private def searchEntries(query : String) : Iterable[(Document, Double)] = {
    val q = new QueryParser(Version.LUCENE_36, "__blob", analyzer).parse(query)
    val hitsPerPage = 10
    val reader = IndexReader.open(index)
    val searcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(hitsPerPage, true)
    searcher.search(q, collector)
    val hits : Array[ScoreDoc] = collector.topDocs.scoreDocs
    val docs = hits.map(hit => (searcher.doc(hit.doc), hit.score.toDouble))
    searcher.close()
    docs
  }

  def documentToEntry(document: Document): Option[BibTeXEntry] = {
    import scala.collection.JavaConversions._
    val em : Map[String,MString] = document.getFields().collect{
      case f if !f.name.startsWith("__") =>
        (f.name -> MString.fromJava(f.stringValue))
    }.toMap

    val optKey = document.get("__key") match {
      case null => None
      case ""   => None
      case s    => Some(s)
    }

    val kind = BibTeXEntryTypes.withName(document.get("__type"))

    BibTeXEntry.fromEntryMap(kind, optKey, em, console ! Error(_))
  }

  private def documentToSearchResult(document : Document, score: Double) : Option[SearchResult] = {
    documentToEntry(document).map(entry => SearchResult(entry, Set(source), score))
  }

  def clear() = {
    initializeIndex()
  }

}

trait LuceneRAMBackend extends LuceneBackend {

  def getLuceneIndex = new RAMDirectory
}

trait LuceneHDDBackend extends LuceneBackend {

  protected val cacheDir: File

  override def clear() = {
    import org.apache.commons.io.FileUtils
    import java.io.IOException
    try {
      FileUtils.deleteDirectory(cacheDir)
      initializeIndex()
    } catch {
      case ioe : IOException =>
        console ! Warning(ioe.getLocalizedMessage)
    }
  }

  def getLuceneIndex = FSDirectory.open(cacheDir)
}

trait LuceneSearchProvider extends SearchProvider {
  this: LuceneBackend =>

  override def preStart() {
    initializeIndex()
  }

  override def search(terms: List[String]): SearchResults = {
    val query = terms.mkString(" ").trim
    if(query.isEmpty) {
      SearchResults(Nil)
    } else {
      SearchResults(searchLucene(query))
    }
  }

}
