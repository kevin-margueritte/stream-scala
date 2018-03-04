import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.kmargueritte",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Simple Stream",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalacheck % Test
  )
