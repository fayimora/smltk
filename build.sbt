name := "Scala Machine Learning Toolkit"

organization := "com.fayimora.smltk"

version := "0.0.1"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc(),
  "org.scalanlp" %% "breeze" % "0.10",
  "org.scalanlp" %% "breeze-natives" % "0.10"
)

initialCommands := "import com.fayimora.smltk._"
