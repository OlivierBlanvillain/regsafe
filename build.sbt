Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(List(
  scalaVersion := "3.1.2-RC1",
  organization := "in.nvilla",
  licenses := Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  homepage := Some(url("https://github.com/OlivierBlanvillain/regsafe")),
  developers := List(
    Developer(
      "OlivierBlanvillain",
      "Olivier Blanvillain",
      "noreply@github.com",
      url("https://github.com/OlivierBlanvillain/")
    )
  )
))

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches += RefPredicate.StartsWith(Ref.Tag("v"))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "regsafe",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v")
  )
