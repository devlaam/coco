name := "coco"

version := "0.3"

scalaVersion := "2.11.6"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

