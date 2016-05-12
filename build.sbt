name := "matryoshka-playground"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.slamdata" %% "matryoshka-core" % "0.9.0",
  "org.specs2" %% "specs2-core" % "3.8" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.8" % "test"
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)

scalacOptions in Test ++= Seq(
  "-Yrangepos"
)


