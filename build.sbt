name := "bibimbap"

version := "1.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

libraryDependencies += "jline" % "jline" % "0.9.94"

libraryDependencies += "org.apache.lucene" % "lucene-core" % "3.6.0"

resolvers += "repo.codahale.com" at "http://repo.codahale.com"

libraryDependencies += "com.codahale" %% "jerkson" % "0.5.0"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("bibimbap.Main")

proguardOptions += "-keep class org.codehaus.** { *; }"
