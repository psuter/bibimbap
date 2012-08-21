seq(assemblySettings : _*)

organization := "net.psuter"

name := "bibimbap"

version := "0.0.1"

scalaVersion := "2.10.0-M6"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

//fork := true

javaOptions in (Test, run) += "-Djline.shutdownhook=false"

//resolvers += "repo.codahale.com" at "http://repo.codahale.com"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
//    "org.scalatest" %% "scalatest" % "1.8" % "test",
    "jline" % "jline" % "2.7",
    "org.apache.lucene" % "lucene-core" % "3.6.0",
    "commons-io" % "commons-io" % "2.4",
    "org.apache.commons" % "commons-lang3" % "3.1",
//    "com.codahale" %% "jerkson" % "0.5.0",
    "com.typesafe.akka" % "akka-actor" % "2.1-M1"
)

mainClass in (Compile, run) := Some("bibimbap.Main")
