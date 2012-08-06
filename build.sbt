seq(assemblySettings : _*)

organization := "net.psuter"

name := "bibimbap"

version := "0.0.1"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "jline" % "jline" % "0.9.94"

libraryDependencies += "org.apache.lucene" % "lucene-core" % "3.6.0"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

resolvers += "repo.codahale.com" at "http://repo.codahale.com"

libraryDependencies += "com.codahale" %% "jerkson" % "0.5.0"
