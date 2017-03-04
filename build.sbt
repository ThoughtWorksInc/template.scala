libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided

libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.patch)

libraryDependencies += "org.scalameta" %% "scalameta" % "1.6.0"

