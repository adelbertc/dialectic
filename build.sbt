name := "dialectic"

description := "*Kanren implementations in Scala"

organization := "com.adelbertc"

scalaVersion in ThisBuild := "2.11.4"

licenses in ThisBuild ++= Seq(("BSD New", url("http://opensource.org/licenses/BSD-3-Clause")))

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Xlog-reflective-calls",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-value-discard"
)

scalacOptions in compile ++= Seq(
  "-Xfatal-warnings",
  "-Ywarn-unused-import"
)

lazy val micro = project.in(file("micro"))

lazy val example = project.in(file("example")).dependsOn(micro)
