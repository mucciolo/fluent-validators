ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "fluent-validators",
    idePackagePrefix := Some("com.mucciolo"),
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.scalatest" %% "scalatest" % "3.2.13" % "test"
    )
  )
