val scala3Version = "3.4.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    coverageEnabled   := true,
    name              := "lalalang",
    version           := "0.1.0-SNAPSHOT",
    scalaVersion      := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-language:implicitConversions",
      "-feature",
      "-deprecation",
      "-unchecked",
      // "-rewrite",
      "-source",
      "future-migration",
      // "-Vprofile",
      "-Xfatal-warnings",
      "-Wunused:all",
      "-Wvalue-discard",
      // "-Yexplicit-nulls",
      // "-Ysafe-init",
      "-Ykind-projector"
    ),
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "parsley"          % "4.2.9",
      "com.lihaoyi"       %% "pprint"           % "0.8.1",
      "org.scalameta"     %% "munit"            % "1.0.0-M10" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "1.0.0-M10" % Test
    )
  )
