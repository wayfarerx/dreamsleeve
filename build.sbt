import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.wayfarerx",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "dreamsleeve",
    libraryDependencies += zeroAllocationHashing,
    libraryDependencies += scalaTest % Test
  )
