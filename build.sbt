val scala3Version = "3.7.1-RC2"

Compile / run / fork := true

lazy val root = project
  .in(file("."))
  .settings(
    coverageEnabled   := true,
    name              := "lalalang",
    version           := "0.1.0-SNAPSHOT",
    scalaVersion      := scala3Version,
    usePipelining     := true,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-language:implicitConversions",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-preview",
      // "-rewrite",
      // "-source",
      // "future",
      // "-language",
      // "experimental.modularity",
      // "-Vprofile",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Wnonunit-statement",
      // "-Xfatal-warnings",
      // "-Yexplicit-nulls",
      "-Wsafe-init",
      "-Xkind-projector"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel"     %% "cats-mtl"         % "1.5.0",
      "org.typelevel"     %% "cats-effect"      % "3.6.1",
      "com.github.j-mie6" %% "parsley"          % "5.0.0-M15",
      "com.lihaoyi"       %% "pprint"           % "0.9.0",
      "tf.tofu"           %% "tofu-kernel"      % "0.13.7",
      "org.scalameta"     %% "munit"            % "1.1.1" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "1.1.0" % Test
    )
  )
