name := "Scala Machine Learning Toolkit"

organization := "smltk"

version := "0.0.1"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc(),
  "org.scalanlp" %% "breeze" % "0.11-M0",
  "org.scalanlp" %% "breeze-natives" % "0.11-M0",
  "com.github.tototoshi" %% "scala-csv" % "1.1.2"
)

initialCommands := "import smltk._"
