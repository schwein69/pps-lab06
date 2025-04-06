val scala3Version = "3.3.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pps-code-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    javacOptions ++= Seq("-source", "17"),

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test
  )
