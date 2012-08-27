package bibimbap
package bibtex

import bibimbap.data._
import bibimbap.strings._

import scala.io.Source

// Still TODO :
//   - make parser completely error-resilient.
//   - ideally, take error reporting function as constructor argument
//   - parse string constants, maintain environment
//   - parse @COMMENT, etc.

class BibTeXParser(src : Source, error : String=>Unit) {
  case class BibTeXParseError(msg : String) extends Exception(msg)
  private val lexer = new Lexer
  private case class RawEntry(kind : String, key : String, pairs : Map[String,String])

  def entries : Stream[BibTeXEntry] = rawEntries.flatMap { raw =>
    val newMap : Map[String,MString] = raw.pairs.mapValues(s => MString.fromJava(s))
    BibTeXEntry.fromEntryMap(newMap.updated("type", MString.fromJava(raw.kind)))
  }

  private def rawEntries : Stream[RawEntry] = rawEntriesOpt.flatten

  private def rawEntriesOpt : Stream[Option[RawEntry]] = {
    if(lastToken == EOF()) {
      Stream.empty
    } else {
      Stream.cons(parseEntry, rawEntriesOpt)
    }
  }

  private def parseError(msg : String) : Nothing = {
    throw new BibTeXParseError(msg)
  }
  private def tryOrSkipUntil[A](block : =>A)(eatUntil : Token=>Boolean) : Option[A] = {
    try {
      Some(block)
    } catch {
      case pe : BibTeXParseError => {
        error(pe.getMessage)
        while(!eatUntil(lastToken) && lastToken != EOF()) {
          nextToken
        }
        None
      }
    }
  }

  private var lastToken : Token = null
  private def nextToken : Token = {
    lastToken = lexer.nextToken 
    lastToken match {
      case ERROR(msg) => parseError(msg)
      case good => good
    } 
  }

  private def eat(tester : Token=>Boolean) : Unit = {
    if(tester(lastToken)) {
      nextToken
    } else {
      parseError("Unexpected token : " + lastToken)
    }
  } 

  private var firstTime : Boolean = true
  private def parseEntry : Option[RawEntry] = {
    if(firstTime) {
      firstTime = false
      nextToken
    }
    tryOrSkipUntil {
      eat(_ == AT())
      val kind = parseID.toLowerCase
      eat(_ == BLOCKSTART())
      val key = parseID
      eat(_ == COMMA())

      var pairs : List[(String,String)] = Nil

      while(lastToken != BLOCKEND()) {
        val pairOpt = tryOrSkipUntil(parsePair)(t => t == COMMA() || t == BLOCKEND())
        pairOpt.foreach(pair => pairs = pair :: pairs)

        while(lastToken == COMMA()) {
          nextToken
        }
      } 
      eat(_ == BLOCKEND())
      new RawEntry(kind, key, pairs.toMap)
    }(_ == AT())
  }

  private def parseID : String = lastToken match {
    case ID(value) => nextToken; value
    case _ => parseError("Expected : identifier")     
  }

  private def parseString : String = {
    val s1 = parseSingleString

    val builder = new StringBuilder
    builder.append(s1.toString)

    while(lastToken == SHARP()) {
      nextToken
      val s2 = parseSingleString
      builder.append(s2)
    }

    builder.toString
  }

  private def parseSingleString : String = lastToken match {
    case STRING(value) => nextToken; value
    case NUM(value) => nextToken; value.toString
    case _ => parseError("Expected : value, found : " + lastToken)
  }


  private def parsePair : (String,String) = {
    val key = parseID.toLowerCase
    eat(_ == EQUALS())
    val value = parseString
    (key -> value)  
  }

  // Below is point is where the lexer sits.

  private sealed abstract class Token(rep : String) {
    private var pos : Int = -1
    def setPos(p : Int) : this.type = {
      pos = p
      this
    }
    def position : Int = pos
    override def toString : String = rep
  }
  private case class EOF() extends Token("EOF")
  private case class AT() extends Token("@")
  private case class BLOCKSTART() extends Token("{")
  private case class BLOCKEND() extends Token("}")
  private case class COMMA() extends Token(",")
  private case class EQUALS() extends Token("=")
  private case class SHARP() extends Token("#")
  private case class ID(value : String) extends Token("identifier[" + value + "]")
  private case class STRING(value : String) extends Token("value")
  private case class NUM(value : Int) extends Token("integer")
  private case class ERROR(reason : String) extends Token("Error: " + reason)

  private class Lexer {
    private var lastChar : Char = _
    private var previousChar : Char = _
    private var tokenPos : Int = -1
    private var currentToken : Token = _
    private val builder = new StringBuilder
    private var inEntry : Boolean = false
    private var inEntryBlock : Boolean = false
    private val EndOfFile : Char = Char.MaxValue

    private def nextChar : Char = {
      def readChar: Char = if(src.hasNext) {
        src.next
      } else {
        EndOfFile
      }

      if(lastChar == EndOfFile) EndOfFile else {
        lastChar = readChar
        previousChar = if ((previousChar == '\r') && (lastChar == '\n')) readChar else lastChar
        lastChar = if (previousChar == '\r') '\n' else previousChar
        lastChar
      }
    }

  
    private var firstCall : Boolean = true
    def nextToken : Token = {
      if(firstCall) {
        nextChar
        firstCall = false
      }

      while(!inEntry && lastChar != '@' && lastChar != EndOfFile) {
        nextChar 
      }
      while(isWhitespace(lastChar)) {
        nextChar
      }
    
      readToken
    } 

    private def isWhitespace(char : Char) : Boolean = Character.isWhitespace(char)
    private def isValidIDFirst(char : Char) : Boolean = Character.isLetter(char)
    private def isValidIDNonFirst(char : Char) : Boolean = char match {
      case _ if Character.isLetterOrDigit(char) => true
      case ':' | '/' | '-' | '_' => true
      case '=' | '{' | '}' | '"' => false
      case _ => false
    }
    private def isValidDigit(char : Char) : Boolean = Character.isDigit(char)

    private def readToken : Token = {
      tokenPos = src.pos

      lastChar match {
        case EndOfFile => EOF().setPos(tokenPos)
        case '@' => {
          inEntry = true
          nextChar
          AT().setPos(tokenPos)
        }
        case '{' if !inEntryBlock => {
          inEntryBlock = true
          nextChar
          BLOCKSTART().setPos(tokenPos)
        }
        case '}' if inEntryBlock => {
          inEntryBlock = false
          inEntry = false
          nextChar
          BLOCKEND().setPos(tokenPos)
        }
        case '=' => {
          nextChar
          EQUALS().setPos(tokenPos)
        }
        case ',' => {
          nextChar
          COMMA().setPos(tokenPos)
        }
        case '#' => {
          nextChar
          SHARP().setPos(tokenPos)
        }
        // String blocks
        case ('{' | '"') if inEntryBlock => {
          val matching : Char = lastChar match {
            case '{' => '}'
            case '"' => '"'
          }

          var error : Option[String] = None

          // to track opening braces
          var openCount : Int = 0

          builder.setLength(0)
          nextChar

          while(!(openCount == 0 && lastChar == matching) && error.isEmpty) {
            if(lastChar == '\n' || lastChar == EndOfFile) {
              error = Some("Unterminated or linebreak in string.")
            } else {
              if(lastChar == '{') {
                openCount += 1
              } else if(lastChar == '}') {
                openCount -= 1
                if(openCount < 0) {
                  error = Some("Unmatched } in string.")
                }
              }
              builder.append(lastChar)
              nextChar 
            }
          }
          nextChar
          (error.map(e => ERROR(e)).getOrElse {
            if(openCount > 0) {
              ERROR("Unclosed { in string.")
            } else {
              STRING(builder.toString) 
            }
          }).setPos(tokenPos)
        }

        case _ if isValidIDFirst(lastChar) => {
          builder.setLength(0)
          do {
            builder.append(lastChar)
          } while(isValidIDNonFirst(nextChar)) // side-effecting tests forever <3
          ID(builder.toString).setPos(tokenPos)
        }

        case '0' => {
          nextChar
          NUM(0).setPos(tokenPos)
        }
        case _ if isValidDigit(lastChar) => {
          builder.setLength(0)
          do {
            builder.append(lastChar)
          } while(isValidDigit(nextChar)) 
          val value = BigInt(builder.toString).toInt
          NUM(value).setPos(tokenPos)
        }
      }
    }
  } 
}
