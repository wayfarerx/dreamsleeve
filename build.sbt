import Dependencies._

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.wayfarerx",
      scalaVersion := "2.12.1",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "dreamsleeve",
    libraryDependencies += cats,
    libraryDependencies += parboiled,
    libraryDependencies += scalaTest % Test
  )
