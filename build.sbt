val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finalibre-marketdata",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= List(
      "com.typesafe" % "config" % "1.4.1",
      "org.scala-lang.modules" % "scala-xml_3" % "2.0.1",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "com.softwaremill.sttp.client3" %% "core" % "3.3.13" % "test",
      "commons-io" % "commons-io" % "2.11.0" % "test"
    )
  )
