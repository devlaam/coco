name := "coco"

version := "0.4"

scalaVersion := "2.11.7"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.9"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % "test"

organization := "com.github.devlaam"

publishMavenStyle := true

