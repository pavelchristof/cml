organization := "cml"

name := "cml"

version := "0.1.1-SNAPSHOT"

scalaVersion := "2.10.5"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.0-RC6",
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)
