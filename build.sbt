name := "coco"

version := "0.3"

scalaVersion := "2.11.2"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

lazy val root = (project in file(".")).addPlugins(PlayScala)

