package bibimbap
package dblp

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.annotations.Column
import org.squeryl.internals.StatementWriter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.H2Adapter
import org.squeryl.adapters.MySQLAdapter
import java.sql.DriverManager

import java.util.Date

/** This class represents an entry from the DBLP database. It is of course very
 * close to the more general entry types defined in the data package, but
 * should still not be confused with any of them. */
class Entry (
  val id : Long,
  @Column("DBLPKey")
  val key : String,
  val kind : String,
  val mdate : Date,
  val allAuthors : Option[String],
  val authorCount : Int,
  val allEditors : Option[String],
  val editorCount : Int,
  val title : Option[String],
  val booktitle : Option[String],
  val pages : Option[String],
  val year : Option[Int],
  val address : Option[String],
  val journal : Option[String],
  val volume : Option[String],
  val number : Option[String],
  val month : Option[String],
  val url : Option[String],
  val ee : Option[String],
  val cdrom : Option[String],
  val publisher : Option[String],
  val note : Option[String],
  val crossref : Option[String],
  val isbn : Option[String],
  val series : Option[String],
  val school : Option[String],
  val chapter : Option[Int]
  ) {

  def this() = this(
    -1L, "error", "error", new Date(), Some(""), 0, Some(""), 0, Some(""),
    Some(""), Some(""), Some(0), Some(""), Some(""), Some(""), Some(""),
    Some(""), Some(""), Some(""), Some(""), Some(""), Some(""), Some(""),
    Some(""), Some(""), Some(""), Some(0)
  )
}

/** Represents a person: either an author or an editor. */
class Person (val id : Long, val name : String) 

class Authorship(val authorId : Long, val entryId : Long, @Column("authorOrder") val order : Int) 

class Editorship(val editorId : Long, val entryId : Long, @Column("editorOrder") val order : Int) 

object DB {
  private var instance : DB = null
  private var usedSettings : Settings = null

  def apply(settings : Settings, create : Boolean = false) : DB = {
    if(instance == null) {
      instance = new DB(settings, create)
      usedSettings = settings
    } else if(settings != usedSettings) {
      sys.error("Cannot create DBLP database twice with different settings.")
    }
    instance
  }

  object DBLP extends Schema {
    val entries = table[Entry]
  
    on(entries)(e => declare (
      e.id is (primaryKey, unique, indexed),
      e.allAuthors is (dbType("text")),
      e.allEditors is (dbType("text")),
      e.key is (indexed, dbType("varchar(255)")),
      e.title is (dbType("text")),
      e.booktitle is (indexed, dbType("varchar(255)")),
      e.journal is (dbType("varchar(255)")),
      e.url is (dbType("text")),
      e.ee is (dbType("text")),
      e.publisher is (dbType("varchar(255)")),
      e.note is (dbType("text")),
      e.school is (dbType("varchar(255)")),
      e.series is (dbType("varchar(255)"))
    ))

    val persons = table[Person]

    on(persons)(p => declare (
      p.id is (primaryKey, unique, indexed),
      p.name is (indexed, dbType("varchar(127)"))
    ))

    val authorship = table[Authorship]

    on(authorship)(as => declare (
      columns(as.entryId, as.authorId) are(indexed)
    ))

    val editorship = table[Editorship]
    
    on(editorship)(es => declare (
      columns(es.entryId, es.editorId) are(indexed)
    ))
  }
}

class DB(settings : Settings = DefaultSettings, create : Boolean = false) {
  val entries = DB.DBLP.entries
  val persons = DB.DBLP.persons
  val authorship = DB.DBLP.authorship
  val editorship = DB.DBLP.editorship

  private def init() {
    settings.get("dblp", "db.type").get match {
      case "h2" | "H2" => {
        Class.forName("org.h2.Driver")

        SessionFactory.concreteFactory = Some(() => {
          Session.create(
            DriverManager.getConnection(
              settings.get("dblp", "db.dsn").get,
              settings.get("dblp", "db.username").get,
              settings.get("dblp", "db.password").get),
            new H2Adapter
          )
        })
      }

      case "MySQL" | "mysql" | "MySql" => {
        Class.forName("com.mysql.jdbc.Driver")

        SessionFactory.concreteFactory = Some(() => {
          Session.create(
            DriverManager.getConnection(
              settings.get("dblp", "db.dsn").get,
              settings.get("dblp", "db.username").get,
              settings.get("dblp", "db.password").get),
            new MySQLAdapter
          )
        })
      }

      case elze => sys.error("Cannot initialize DBLP database. Unsupported DB type : " + elze + ".")
    }
  }

  private def createTables() {
    transaction {
      DB.DBLP.create
    }
  }

  init()
  if(create) {
    createTables
  }
}
