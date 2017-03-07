addSbtPlugin("com.thoughtworks.sbt-best-practice" % "sbt-best-practice" % "latest.release")

dependsOn(RootProject(uri("https://github.com/sbt/sbt-release.git#b0fc791a85b0978f919ad7e6616af59853842316")))
