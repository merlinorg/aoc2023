ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2023",
    idePackagePrefix := Some("org.merlin.aoc2023"),
    libraryDependencies +=  "org.scalaz" %% "scalaz-core" % "7.3.7",
  )
