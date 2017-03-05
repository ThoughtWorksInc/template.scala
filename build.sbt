libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided

libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.patch)

libraryDependencies += "org.scalameta" %% "scalameta" % "1.6.0"

crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1")

organization in ThisBuild := "com.thoughtworks.template"

name := "template"

scalaOrganization := "org.typelevel"

scalacOptions += "-Yliteral-types"