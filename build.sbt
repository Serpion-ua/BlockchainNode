ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "BlockchainNode"
  )

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-client" % "1.0.0-M41",
  "org.http4s" %% "http4s-circe" % "1.0.0-M44",
  "org.http4s" %% "http4s-ember-server" % "1.0.0-M44",
  "org.http4s" %% "http4s-ember-client" % "1.0.0-M44",
  "org.http4s" %% "http4s-dsl" % "1.0.0-M44",
  "org.http4s" %% "http4s-circe" % "1.0.0-M44",
  "io.circe" %% "circe-core" % "0.14.10",
  "io.circe" %% "circe-parser" % "0.14.10",
  "io.circe" %% "circe-generic" % "0.14.10",
  "org.typelevel" %% "cats-effect" % "3.5.7",
  "org.typelevel" %% "log4cats-slf4j" % "2.7.0",
  "ch.qos.logback" % "logback-classic" % "1.5.17",
  "pl.iterators" %% "kebs-tagged" % "2.0.0",
  "org.bouncycastle" % "bcprov-jdk18on" % "1.80",
  "com.github.pureconfig" %% "pureconfig" % "0.17.8",
  "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test,
  "org.typelevel" %% "scalacheck-effect-munit" % "2.0-9366e44" % Test,
  "org.scalamock" %% "scalamock-cats-effect" % "7.3.0-RC1" % Test,
  "org.scalamock" %% "scalamock" % "6.2.0" % Test
)