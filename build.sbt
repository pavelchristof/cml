organization := "cml"

name := "cml"

version := "0.1.2-SNAPSHOT"

scalaVersion := "2.10.5"

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.0-RC6",
  "org.scalaz" %% "scalaz-core" % "7.2.0-SNAPSHOT",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
  "org.apache.spark" %% "spark-core" % "1.3.0" % "provided",
  "org.apache.spark" %% "spark-core" % "1.3.0" % "test"
)
