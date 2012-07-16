name := "bibimbap"

version := "1.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "jline" % "jline" % "0.9.94"

resolvers += "repo.codahale.com" at "http://repo.codahale.com"

libraryDependencies += "com.codahale" %% "jerkson" % "0.5.0"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.3",
  "commons-httpclient" % "commons-httpclient" % "3.1",
  "commons-io" % "commons-io" % "2.1",
  "commons-logging" % "commons-logging" % "1.1.1"
)

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("bibimbap.Main")

proguardOptions += "-keep class org.apache.commons.** { *; }"
