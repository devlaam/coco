scalaVersion        :=   "2.13.4"
//scalacOptions       ++=   Seq("-feature","-deprecation","-unchecked","-Ywarn-unused","-Xlint")
//name                :=   "coco"
//organization        :=   "devlaam"
//version             :=   "0.6.14"

EclipseKeys.useProjectId := true

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  name                :=   "coco",
  organization        :=   "devlaam",
  version             :=   "0.7.0",
  //scalacOptions       ++=   Seq("-feature","-deprecation","-unchecked","-Ywarn-unused","-Xlint"),
  scalacOptions       ++=   Seq("-feature","-deprecation","-unchecked"),
  testFrameworks      +=    new TestFramework("utest.runner.Framework") )
  
val jvmSettings = Seq(
  libraryDependencies += "org.typelevel" %% "jawn-parser" % "1.0.4-f1"  withSources(), 
  libraryDependencies +=   "com.lihaoyi" %%% "utest" % "0.7.5" % "test" withSources() )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  libraryDependencies +=   "com.lihaoyi" %%% "utest" % "0.7.4" % "test" withSources() )

lazy val coco = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full) 
    .in(file("."))
    .settings(sharedSettings)
    .jvmSettings(jvmSettings)
    .jsSettings(jsSettings)

lazy val cocoJVM = coco.jvm
lazy val cocoJS  = coco.js
