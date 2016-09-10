name := "coco"

version := "0.5"

scalaVersion := "2.11.8"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "src"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.9"  withSources()

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" 

organization := "com.github.devlaam"

publishMavenStyle := true

