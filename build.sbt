import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx",
  scalaVersion := "2.12.1",
  version := "0.1.0-SNAPSHOT"
)

lazy val io = (project in file("shared/io")).
  settings(
    common,
    name := "dreamsleeve-io",
    libraryDependencies += cats,
    libraryDependencies += scalaTest % Test
  )

lazy val data = (project in file("shared/data")).
  dependsOn(io).
  settings(
    common,
    name := "dreamsleeve-data",
    libraryDependencies += cats,
    libraryDependencies += parboiled,
    libraryDependencies += scalaTest % Test
  )
