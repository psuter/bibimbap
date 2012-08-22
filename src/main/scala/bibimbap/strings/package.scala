package bibimbap

package object strings {
  import language.implicitConversions

  implicit def str2mstr(str : String) : MString = MString.fromJava(str)
  implicit def seq2seq(ss : Seq[String]) : Seq[MString] = ss.map(MString.fromJava)
  implicit def opt2opt(os : Option[String]) : Option[MString] = os.map(MString.fromJava)
  // implicit def mstr2str(mst : MString) : String = mst.toJava
}
