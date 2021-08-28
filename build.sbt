val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finalibre-marketdata",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= List(
      "com.typesafe" % "config" % "1.4.1",
      "commons-io" % "commons-io" % "2.11.0",
      "org.scala-lang.modules" % "scala-xml_3" % "2.0.1",
      "com.softwaremill.sttp.client3" %% "core" % "3.3.13",
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
      "com.h2database" % "h2" % "1.4.200" % Test,
      "org.tpolecat" %% "doobie-core" % "0.13.4" % Test,
      "org.tpolecat" %% "doobie-h2" % "0.13.4" % Test

/*      "com.typesafe.slick" % "slick_2.13" % "3.3.3" % Test,
      "org.slf4j" % "slf4j-nop" % "1.6.4" % Test,
      "com.typesafe.slick" % "slick-hikaricp_2.13" % "3.3.3" % Test*/
    )
  )
