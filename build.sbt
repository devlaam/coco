EclipseKeys.useProjectId := true

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  scalaVersion        :=   "2.12.6",
  name                :=   "coco",
  version             :=   "0.6.0",
  organization        :=   "devlaam",
  scalacOptions       ++=   Seq("-feature","-deprecation","-unchecked"),
  publish             :=    {},
  publishLocal        :=    {},
  libraryDependencies +=   "com.lihaoyi" %%% "utest" % "0.4.7" % "test" withSources(), 
  testFrameworks      +=    new TestFramework("utest.runner.Framework") )
  
val jvmSettings = Seq(
    libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.12.1"  withSources() )

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
