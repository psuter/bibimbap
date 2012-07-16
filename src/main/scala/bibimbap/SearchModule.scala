package bibimbap

import bibimbap.data._

abstract class SearchModule(settings : Settings) extends Module(settings) {
  val dataSourceName : String
  def searchAction : Action[SearchResult]
}
