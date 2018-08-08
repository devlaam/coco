scalaVersion        :=   "2.12.6"
scalacOptions       ++=   Seq("-feature","-deprecation","-unchecked")
name                :=   "coco"
organization        :=   "devlaam"
version             :=   "0.6.11"

EclipseKeys.useProjectId := true

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  name                :=   "coco",
  organization        :=   "devlaam",
  version             :=   "0.6.11",
  scalacOptions       ++=   Seq("-feature","-deprecation","-unchecked"),
  libraryDependencies +=   "com.lihaoyi" %%% "utest" % "0.4.7" % "test" withSources(), 
  testFrameworks      +=    new TestFramework("utest.runner.Framework") )
  
val jvmSettings = Seq(
    libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.12.2-f5"  withSources() )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true )

lazy val coco = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full) 
    .in(file("."))
    .settings(sharedSettings)
    .jvmSettings(jvmSettings)
    .jsSettings(jsSettings)

lazy val cocoJVM = coco.jvm
lazy val cocoJS  = coco.js
