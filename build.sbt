val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pps-23-24-lab03",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "dev.zio" %% "zio" % "2.1-RC1",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test
  )
