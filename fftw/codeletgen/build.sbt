lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.12.3"
    )),
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0"

scalacOptions ++= Seq( // "-Xfatal-warnings",
  "-Ypartial-unification" )