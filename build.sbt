ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2023",
    idePackagePrefix := Some("org.merlin.aoc2023"),
    libraryDependencies ++=  Seq(
      "org.scalaz" %% "scalaz-core" % "7.3.7",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    ),
    javaOptions += "-Xmx16G",
  )
