name := "dialectic-micro"

val scalazVersion = "7.1.0"

val specs2Version = "2.4.15"

libraryDependencies ++= Seq(
  "org.scalaz"  %% "scalaz-core"                % scalazVersion,
  "org.scalaz"  %% "scalaz-scalacheck-binding"  % scalazVersion   % "test",
  "org.specs2"  %% "specs2-core"                % specs2Version   % "test",
  "org.specs2"  %% "specs2-scalacheck"          % specs2Version   % "test"
)

initialCommands :=
  """
  import dialectic.micro._
  import dialectic.micro.Micro._
  """
