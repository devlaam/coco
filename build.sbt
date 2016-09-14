scalaVersion in ThisBuild := "2.11.8"

//enablePlugins(ScalaJSPlugin)

EclipseKeys.useProjectId := true

lazy val root = project.in(file(".")).
  aggregate(cocoJS, cocoJVM).
  settings(
    publish := {},
    publishLocal := {}
  )


lazy val coco = crossProject.in(file(".")).
  settings(
    name := "coco",
    version := "0.5.3",
    organization := "devlaam",
    scalacOptions ++= Seq("-feature","-deprecation","-unchecked"),
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.3" % "test" withSources(), 
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.9"  withSources()
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val cocoJVM = coco.jvm
lazy val cocoJS  = coco.js


//scalaSource in Compile := baseDirectory.value / "src" / "main"

//scalaSource in Test := baseDirectory.value / "src" / "test"

//publishMavenStyle := true

