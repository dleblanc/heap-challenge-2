name := """hello-scala"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0-M3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"


//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

fork in run := true
