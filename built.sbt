name := "coco"

version := "0.2"

scalaVersion := "2.10.3"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

libraryDependencies += "com.typesafe.play" %% "play" % "2.2.2-RC1"

libraryDependencies += "com.typesafe.play" %% "play-test" % "2.2.2-RC1"