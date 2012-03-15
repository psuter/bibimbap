name := "bibimbap"

version := "1.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies ++= Seq(
  "jline" % "jline" % "0.9.94",
  "org.squeryl" %% "squeryl" % "0.9.5-RC1",
  "com.h2database" % "h2" % "1.2.127",
  "mysql" % "mysql-connector-java" % "5.1.15",
  "commons-codec" % "commons-codec" % "1.3",
  "commons-httpclient" % "commons-httpclient" % "3.1",
  "commons-io" % "commons-io" % "2.1",
  "commons-logging" % "commons-logging" % "1.1.1"
)

