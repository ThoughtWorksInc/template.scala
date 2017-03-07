addSbtPlugin("com.thoughtworks.sbt-best-practice" % "sbt-best-practice" % "latest.release")

dependsOn(RootProject(uri("https://github.com/atry/sbt-release.git#cross-check-snapshot")))