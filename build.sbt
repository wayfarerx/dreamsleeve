import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx",
  scalaVersion := "2.12.1",
  version := "0.1.0-SNAPSHOT"
)

lazy val data = (project in file("shared/data")).
  settings(
    common,
    name := "dreamsleeve-data",
    libraryDependencies += catsCore,
    libraryDependencies += catsFree,
    libraryDependencies += scodecCore,
    libraryDependencies += parboiled,
    libraryDependencies += scalaTest % Test
  )
