import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx",
  scalaVersion := "2.12.1",
  version := "0.1.0-SNAPSHOT"
)

lazy val core = (project in file("shared/core")).
  settings(
    common,
    name := "dreamsleeve-core",
    libraryDependencies += cats,
    libraryDependencies += scalaTest % Test,
    addCompilerPlugin(kinds)
  )

lazy val io = (project in file("shared/io")).
  dependsOn(core).
  settings(
    common,
    name := "dreamsleeve-io",
    libraryDependencies += cats,
    libraryDependencies += scalaTest % Test
  )

lazy val data = (project in file("shared/data")).
  dependsOn(core, io).
  settings(
    common,
    name := "dreamsleeve-data",
    libraryDependencies += cats,
    libraryDependencies += parboiled,
    libraryDependencies += scalaTest % Test
  )
