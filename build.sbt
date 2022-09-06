val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    coverageEnabled := true,
    name            := "lalalang",
    version         := "0.1.0-SNAPSHOT",
    scalaVersion    := scala3Version,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-Xfatal-warnings",
      // "-Yexplicit-nulls",
      "-Ykind-projector",
      // "-Ysafe-init",
      "-rewrite",
      "-source",
      "future-migration",
      "-Vprofile"
    ),
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "parsley"          % "3.3.10",
      "org.scalameta"     %% "munit"            % "1.0.0-M6" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "1.0.0-M6" % Test
    )
  )
