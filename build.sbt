val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "lalalang",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "parsley"          % "3.3.10",
      "org.scalameta"     %% "munit"            % "1.0.0-M6" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "1.0.0-M6" % Test
    )
  )
