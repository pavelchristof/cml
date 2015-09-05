
organization := "cml"

name := "cml"

version := "0.3.0-RC1"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)
