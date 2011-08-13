package bibimbap

package object dblp {
  case class InProceedings(
    key : String,
    authors : List[String],
    title : String,
    pageStart : Int,
    pageEnd : Int,
    year : Int,
    bookTitle : String,
    ee : String,
    crossRef : String,
    url : String
  )

  case class Proceedings(
    key : String,
    editors : List[String],
    title : String,
    volume : Int,
    year : Int,
    ee : String,
    isbn : String,
    bookTitle : String,
    series : String,
    publisher : String,
    url : String
  )
}
