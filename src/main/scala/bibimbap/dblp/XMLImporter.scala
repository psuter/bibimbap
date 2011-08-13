package bibimbap
package dblp

import scala.collection.mutable.{Map=>MutableMap}

import scala.io.Source

import java.text.SimpleDateFormat
import java.util.Date

import org.squeryl.PrimitiveTypeMode._

/** This class is used to import the contents of dblp.xml into the database. It
 * works by clearing the database first, so be careful.  It uses the Settings
 * object to determine how to access the database. It assumes the database
 * already exists and the tables have been created according to the schema. */
class XMLImporter(file : String, settings : Settings = DefaultSettings) {
  private val db = DB(settings, false)

  def importPersons : MutableMap[String,Person] = {
    // Dump all entries in persons.
    // TODO also dump in associated tables, maybe that's automatic?
    transaction {
      db.persons.deleteWhere(_.name like "%")
    }

    var nextID : Long = 1L

    val bufferSize : Int = 50000
    var buffer : List[Person] = Nil
    var inBuffer : Int = 0

    val parser = new XMLParser(Source.fromFile(file))

    val pmap : MutableMap[String,Person] = MutableMap.empty

    var total    : Int = 0
    var distinct : Int = 0

    def emptyBuffer {
      if(inBuffer > 0) {
        println("Inserting " + inBuffer + " person entries...")
        try {
          //for(p <- buffer) {
            transaction {
              db.persons.insert(buffer/*p*/)
            }
          //}
        } catch {
          case e => println("Exception occured : " + e)
        }
        println("...done.")
        buffer = Nil
        inBuffer = 0
      }
    }

    def treatPerson(name : String) {
      total += 1
      if(!pmap.isDefinedAt(name)) {
        distinct += 1
        val person = new Person(nextID, name)
        nextID += 1

        pmap(name) = person

        buffer = person :: buffer
        inBuffer += 1
        if(inBuffer >= bufferSize) emptyBuffer
      }
    }

    parser.parsePersons(treatPerson _)

    emptyBuffer

    println("Total names : " + total + ", distinct : " + distinct)

    pmap
  }

  // NOTE: Currently the anew parameter is ignored (as if always true...)
  def importEntries {
    val pmap : MutableMap[String,Person] = importPersons

    val parser = new XMLParser(Source.fromFile(file))

    type Container = parser.Container

    val dateParser = new SimpleDateFormat("yyyy-MM-dd")

    transaction {
      db.entries.deleteWhere(_.key like "%")
    }

    val bufferSize : Int = 50000
    var buffer : List[Entry] = Nil
    var inBuffer : Int = 0

    var nextID : Long = 0

    // Int is order in author/editor list, boolean == isAuthor
    type Assoc = (Entry, Person, Int, Boolean)
    var assocBuffer : List[Assoc] = Nil

    def emptyBuffers {
      if(inBuffer > 0) {
        println("Inserting " + inBuffer + " DBLP entries...")
        try {
          //for(e <- buffer) {
            transaction {
              db.entries.insert(buffer/*e*/)
            }
          //}
        } catch {
          case e => println("Exception occured : " + e)
        }
        println("...done.")

        println("Preparing author/editor associations...")
        var aa : List[Authorship] = Nil
        var ea : List[Editorship] = Nil

        for((entry, person, order, isAuth) <- assocBuffer) {
          // println("Entry.id " + entry.id)
          // println("Person.id " + person.id)
          if(isAuth) {
            aa = (new Authorship(person.id, entry.id, order)) :: aa
          } else {
            ea = (new Editorship(person.id, entry.id, order)) :: ea
          }
        }
        println("...done.")

        println("Inserting author/editor associations...")
        try {
          transaction {
            db.authorship.insert(aa)
          }
        } catch {
          case e => println("Exception occured : " + e)
        }
        try {
          transaction {
            db.editorship.insert(ea)
          }
        } catch {
          case e => println("Exception occured : " + e)
        }
        println("...done.")

        inBuffer = 0
        buffer = Nil
        assocBuffer = Nil
      }
    }

    def treatContainer(c : Container) {
      // println("Got this from parser : " + c)
      // Preparing the data...
      var key : String = ""
      val kind : String = c.name
      var mdate : Date = new Date()
      var authors : List[String] = Nil // this gets reformatted in the end
      var editors : List[String] = Nil // same here
      var title : Option[String] = None
      var booktitle : Option[String] = None
      var pages : Option[String] = None
      var year : Option[Int] = None
      var address : Option[String] = None
      var journal : Option[String] = None
      var volume : Option[String] = None
      var number : Option[String] = None
      var month : Option[String] = None
      var url : Option[String] = None
      var ee : Option[String] = None
      var cdrom : Option[String] = None
      var publisher : Option[String] = None
      var note : Option[String] = None
      var crossref : Option[String] = None
      var isbn : Option[String] = None
      var series : Option[String] = None
      var school : Option[String] = None
      var chapter : Option[Int] = None

      for((k,v) <- c.getAttrs) k match {
        case "key"   => key = v
        case "mdate" => mdate = dateParser.parse(v)
        case _ => println("Skipping attribute " + (k,v))
      }

      for((k,v) <- c.getElems) k match {
        case "author"    => authors = v :: authors
        case "editor"    => editors = v :: editors
        case "title"     => title = Some(v)
        case "booktitle" => booktitle = Some(v)
        case "pages"     => pages = Some(v)
        case "year"      => year = Some(v.toInt)
        case "address"   => address = Some(v)
        case "journal"   => journal = Some(v)
        case "volume"    => volume = Some(v)
        case "number"    => number = Some(v)
        case "month"     => month = Some(v)
        case "url"       => url = Some(v)
        case "ee"        => ee = Some(v)
        case "cdrom"     => cdrom = Some(v)
        case "publisher" => publisher = Some(v)
        case "note"      => note = Some(v)
        case "crossref"  => crossref = Some(v)
        case "isbn"      => isbn = Some(v)
        case "series"    => series = Some(v)
        case "school"    => school = Some(v)
        case "chapter"   => chapter = Some(v.toInt)
        case "cite"      => ;
        case _ => println("Skipping element " + (k,v))
      }

      // val finalAuthors = if(authors.isEmpty) None else Some(authors.reverse.mkString(";"))
      // val finalEditors = if(editors.isEmpty) None else Some(editors.reverse.mkString(";"))

      val entry = new Entry(nextID, key, kind, mdate, title, booktitle, pages, year, address, journal, volume, number, month, url, ee, cdrom, publisher, note, crossref, isbn, series, school, chapter)
      nextID += 1
      
      buffer = entry :: buffer
      inBuffer += 1

      for((a,i) <- authors.reverse.zipWithIndex) {
        assocBuffer = ((entry, pmap(a), i, true)) :: assocBuffer
      }

      for((e,i) <- editors.reverse.zipWithIndex) {
        assocBuffer = ((entry, pmap(e), i, false)) :: assocBuffer
      }

      if(inBuffer >= bufferSize) {
        emptyBuffers
      }
    }

    parser.parseContainers(treatContainer _)

    emptyBuffers
  }
}
