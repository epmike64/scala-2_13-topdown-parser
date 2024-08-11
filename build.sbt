ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

lazy val root = (project in file("."))
  .settings(
    name := "scala-compiler-by-first-set"
  )
