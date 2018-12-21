name := "15puzzle"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.fusesource.jansi" % "jansi" % "1.17.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "org.scalamock" %% "scalamock" % "4.1.0" % Test
)