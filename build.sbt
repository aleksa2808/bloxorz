import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.aleksap"
ThisBuild / organizationName := "aleksap"

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _                            => throw new Exception("Unknown platform!")
}

lazy val javaFXModules =
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

lazy val root = (project in file("."))
  .settings(
    name := "bloxorz",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaFX,
    libraryDependencies ++= javaFXModules.map(
      m => "org.openjfx" % s"javafx-$m" % "12.0.1" classifier osName
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
