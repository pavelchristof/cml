name := "cml"

version := "1.0"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.0-RC6",
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.0"
)