import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.11.8",
      version      := "0.1.0"
    )),
    name := "free-monix-cats",
    scalacOptions ++= Seq("-language:higherKinds"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= Seq (cats, monix, monixCats, specs2core % Test, specs2scalaCheck % Test)
  )
