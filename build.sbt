import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

workbenchSettings

name := "Graph"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.2",
  "org.scala-js" %%% "scala-parser-combinators" % "1.0.2",
  "com.lihaoyi" %%% "scalatags" % "0.5.4"
)

bootSnippet := "org.lazyluke.Main().main(document.getElementById('canvas'));"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

