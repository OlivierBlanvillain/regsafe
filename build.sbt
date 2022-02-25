ThisBuild / scalaVersion := "3.1.2-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "regsafe",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v")
  )
