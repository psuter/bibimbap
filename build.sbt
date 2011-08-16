name := "BibEdit"

version := "1.0"

organization := "net.psuter"

scalaVersion := "2.9.0-1"

libraryDependencies += "jline" % "jline" % "0.9.94"

libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.4"

libraryDependencies += "com.h2database" % "h2" % "1.2.127"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.15"

// libraryDependencies += "org.apache.lucene" % "lucene-core" % "3.3.0"

scalacOptions += "-deprecation"

seq(ProguardPlugin.proguardSettings :_*)

proguardDefaultArgs := Seq("-dontwarn")

proguardOptions ++= Seq(
    "-dontshrink",
    "-dontoptimize",
    "-dontobfuscate",
    "-dontnote",
    "-dontwarn",
    "-ignorewarnings"
)

makeInJarFilter <<= (makeInJarFilter) {
  (makeInJarFilter) => {
    (file) => file match {
      case f if f.endsWith(".jar") => makeInJarFilter(file) + ",!META-INF/**"
      case _ => makeInJarFilter(file)
    }
  }
}
