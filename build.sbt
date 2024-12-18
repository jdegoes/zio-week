val zioVersion            = "2.1.11"
val zioHttpVersion        = "3.0.1"
val zioKafkaVersion       = "2.8.2"
val zioJsonVersion        = "0.7.3"
val zioPreludeVersion     = "1.0.0-RC31"
val zioConfigVersion      = "4.0.2"
val zioLoggingVersion     = "2.3.2"
val logbackClassicVersion = "1.5.11"
val postgresqlVersion     = "42.7.4"
val flywayVersion         = "10.20.0"
val chimneyVersion        = "1.5.0"
val testContainersVersion = "0.41.4"
val zioMockVersion        = "1.0.0-RC12"

lazy val root = (project in file("."))
  .settings(
    inThisBuild(
      List(
        organization := "net.degoes",
        scalaVersion := "3.5.1",
      )
    ),
    name                    := "zio-week",
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql"                 % postgresqlVersion,
      "org.flywaydb"   % "flyway-core"                % flywayVersion,
      "org.flywaydb"   % "flyway-database-postgresql" % flywayVersion,
      "dev.zio"       %% "zio"                        % zioVersion,
      "dev.zio"       %% "zio-http"                   % zioHttpVersion,
      "dev.zio"       %% "zio-kafka"                  % zioKafkaVersion,
      "dev.zio"       %% "zio-config"                 % zioConfigVersion,
      "dev.zio"       %% "zio-config-typesafe"        % zioConfigVersion,
      "dev.zio"       %% "zio-config-magnolia"        % zioConfigVersion,
      "dev.zio"       %% "zio-json"                   % zioJsonVersion,
      "io.scalaland"  %% "chimney"                    % chimneyVersion,
      "dev.zio"       %% "zio-prelude"                % zioPreludeVersion,

      // logging
      "dev.zio"       %% "zio-logging"       % zioLoggingVersion,
      "dev.zio"       %% "zio-logging-slf4j" % zioLoggingVersion,
      "ch.qos.logback" % "logback-classic"   % logbackClassicVersion,

      // test
      "dev.zio"      %% "zio-test"                        % zioVersion,
      "dev.zio"      %% "zio-test-sbt"                    % zioVersion,
      "dev.zio"      %% "zio-test-junit"                  % zioVersion,
      "dev.zio"      %% "zio-mock"                        % zioMockVersion,
      "com.dimafeng" %% "testcontainers-scala-postgresql" % testContainersVersion,
      "dev.zio"      %% "zio-test-magnolia"               % zioVersion,
    ),
    testFrameworks          := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    // try using the `tpolecatScalacOptions` configuration key for any additional compiler flags
    Compile / doc / sources := Seq.empty,
  )
  .enablePlugins(JavaAppPackaging, UniversalPlugin)

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")


