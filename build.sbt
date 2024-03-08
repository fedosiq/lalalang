val scala3Version = "3.4.1-RC1"

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
      // "-source",
      // "future",
      // "-Vprofile",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-Xfatal-warnings",
      // "-Yexplicit-nulls",
      // "-Ysafe-init",
      "-Ykind-projector"
    ),
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "parsley"          % "4.2.9",
      "com.lihaoyi"       %% "pprint"           % "0.8.1",
      "tf.tofu"           %% "tofu"             % "0.12.1" cross CrossVersion.for3Use2_13,
      "org.scalameta"     %% "munit"            % "1.0.0-M11" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "1.0.0-M11" % Test
    )
  )
