name := "cats-effect"

version := "3.5.4"

scalaVersion := "2.13.13"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked"
)