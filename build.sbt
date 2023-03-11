ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.2"

val CatsVer = "2.9.0"
val ScalatestVer = "3.2.15"

lazy val root = (project in file("."))
  .settings(
    name := "fluent-validators",
    idePackagePrefix := Some("com.mucciolo"),
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % CatsVer,
      "org.typelevel" %% "cats-laws" % CatsVer,
      "org.scalatest" %% "scalatest" % ScalatestVer % Test
    )
  )
