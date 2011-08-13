package bibimbap
package dblp

import scala.io.Source
import scala.xml.pull._

/** Parses dblp.xml and produces an iterator of 'Container's, a class that
 * represents a DBLP entry using text tags only. Such entries are later
 * converted into proper DB entries in XMLImporter. */
class XMLParser(source : Source) {
  // This XML parser takes in a dblp.xml file, and turns it into an iterator of "containers".
  class Container(val name : String) {
    private var attrs : List[(String,String)] = Nil
    private var elems : List[(String,String)] = Nil
    
    def addAttr(key : String, value : String) {
      attrs = (key -> value) :: attrs
    }

    def addElem(key : String, value : String) {
      elems = (key -> value) :: elems
    }

    def getAttrs : List[(String,String)] = attrs.reverse

    def getElems : List[(String,String)] = elems.reverse

    override def toString : String = {
      "[" + name + getAttrs.map(p => p._1 + " -> '" + p._2 + "')").mkString(" ", ", ", "") + "]" +
      getElems.map(p => "  " + p._1 + " : " + p._2 + "\n").mkString("\n", "", "")
    }
  }

  private val containerTags : Set[String] = Set(
    "article", "inproceedings", "proceedings", "book", "incollection",
    "phdthesis", "mastersthesis", "www"
  )

  private val elemTags : Set[String] = Set(
    "author", "editor", "title", "booktitle", "pages", "year", "address",
    "journal", "volume", "number", "month", "url", "ee", "cdrom", "cite",
    "publisher", "note", "crossref", "isbn", "series", "school", "chapter"
  )

  private val textMarkupTags : Set[String] = Set(
    "sub", "sup", "i", "tt", "ref"
  )

  // Parse the whole XML file, and every time a container is completely parsed,
  // calls the callback function on it.
  def parseContainers(callback : Container=>Unit) {
    val eventReader = new XMLEventReader(source)

    var inContainer : Boolean = false
    var inElem : Boolean = false
    var currentContainer : Container = null
    var currentElemName : String = null
    var currentText : StringBuilder = null

    def handleEvent(event : XMLEvent) {
      def storeText(txt : String) {
        if(currentText == null) {
          currentText = new StringBuilder
        } 
        currentText.append(txt)
      }
      event match {
        case EvElemStart(_, "dblp", _, _) => ;
        case EvElemEnd(_, "dblp") => ;

        // Entering container tags
        case EvElemStart(_, elem, attrs, _) if containerTags(elem) => {
          if(inContainer) {
            sys.error("Tag encountered : " + elem + " while in container " + currentContainer.name)
          } else {
            inContainer = true
            currentContainer = new Container(elem)
            for(attr <- attrs) {
              currentContainer.addAttr(attr.key, attr.value.toString)
            }
          }
        }

        // Exiting container tags
        case EvElemEnd(_, elem) if containerTags(elem) => {
          inContainer = false
          callback(currentContainer)
          currentContainer = null
        }

        // Entering elem tags
        case EvElemStart(_, elem, _, _) if elemTags(elem) => {
          if(!inContainer) {
            sys.error("Tag encountered : " + elem + " while not in container.")
          } else {
            currentElemName = elem
          }
        }

        // Exiting elem tags
        case EvElemEnd(_, elem) if elemTags(elem) => {
          if(!inContainer) {
            sys.error("End of tag encountered : " + elem + " while not in container.")
          } else if(currentText == null) {
            sys.error("Text empty while closing elem tag.")
          } else {
            currentContainer.addElem(currentElemName, currentText.toString)
            currentElemName = null
            currentText = null
          }
        }

        // Markup tags in text
        case EvElemStart(_, elem, _, _) if textMarkupTags(elem) => storeText("<" + elem + ">")
        case EvElemEnd(_, elem) if textMarkupTags(elem) => storeText("</" + elem + ">")

        case EvText(txt) => storeText(txt)
        case EvComment(_) => ;

        case _ => Console.err.println("Warning : Unhandled XML event : " + event)
      }
    }

    val realHandler = repackText(handleEvent _)

    eventReader.foreach(realHandler)
  }

  def parsePersons(callback : String=>Unit) {
    val eventReader = new XMLEventReader(source)

    var inPerson : Boolean = false
    var currentText : StringBuilder = null
    def handleEvent(event : XMLEvent) {
      def storeText(txt : String) {
        if(currentText == null) {
          currentText = new StringBuilder
        } 
        currentText.append(txt)
      }
      event match {
        case EvElemStart(_, "author", _, _) | EvElemStart(_, "editor", _, _) => inPerson = true
        case EvElemEnd(_, "author") | EvElemEnd(_, "editor") => {
          callback(currentText.toString)
          currentText = null
          inPerson = false
        }

        case EvText(txt) if inPerson => storeText(txt)
        case _ => ;
      }
    }
    val realHandler = repackText(handleEvent _)
    eventReader.foreach(realHandler)
  }

  // Concatenates text events, skips empty text events, and magically resolves
  // some entities.
  private val unknownEntityMsg = "unknown entity"
  private def repackText(handler : XMLEvent=>Unit) : XMLEvent=>Unit = {
    new Function1[XMLEvent,Unit] {
      private var lastWasText : Boolean = false
      private var concatenated : String = ""

      def saveText(txt : String) {
        if(lastWasText) {
          concatenated += txt
        } else {
          lastWasText = true
          concatenated = txt
        }
      }

      override def apply(event : XMLEvent) : Unit = event match {
        case EvText(str) => saveText(str)
        case EvEntityRef(ref) => saveText(EntityMap(ref))
        case EvComment(txt) if txt.contains(unknownEntityMsg) => {
          val theEntity = (txt
            .replace(unknownEntityMsg, "")
            .replace("&","")
            .replace(";","")
            .trim)

          saveText(EntityMap(theEntity))
        }
        case other => {
          if(lastWasText) {
            lastWasText = false
            val txt = concatenated.trim
            if(!txt.isEmpty) {
              handler(EvText(concatenated))
            }
          } 
          handler(other)
        }
      }
    }
  }
}
