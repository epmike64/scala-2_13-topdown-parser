ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

lazy val root = (project in file("."))
  .settings(
    name := "scala-compiler-by-first-set"
  )
