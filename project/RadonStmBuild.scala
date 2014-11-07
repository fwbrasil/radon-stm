import sbt._
import Keys._

object RadonStmBuild extends Build {

    /* Dependencies */
    val junit = "junit" % "junit" % "4.11" % "test"
    val specs2 = "org.specs2" %% "specs2" % "2.4.9" % "test"
    def scalaActors(version: String) = "org.scala-lang" % "scala-actors" % version % "test"

    val customResolvers = Seq(
        "Maven" at "http://repo1.maven.org/maven2/",
        "Typesafe" at "http://repo.typesafe.com/typesafe/releases",
        "Local Maven Repository" at "" + Path.userHome.asFile.toURI.toURL + "/.m2/repository",
        "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
        "fwbrasil.net" at "http://fwbrasil.net/maven/",
        "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
        "Alfesco" at "https://maven.alfresco.com/nexus/content/groups/public/"
    )

    lazy val radonStm =
        Project(
            id = "radon-stm",
            base = file("."),
            settings = Defaults.defaultSettings ++ Seq(
                crossScalaVersions := Seq("2.10.4","2.11.4"),
                scalaVersion := "2.11.4",
                version := "1.7.1",
                // libraryDependencies ++= Seq(junit, specs2, scalaActors),
                libraryDependencies <++= (scalaVersion) { v: String => Seq(junit, specs2, scalaActors(v)) },
                javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
                resolvers ++= customResolvers,
                // publishTo := Some(Resolver.file("file",  file(Path.userHome.absolutePath+"/.m2/repository"))), 
                // publishTo := Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as ("maven") withPermissions ("0644")),
                //*
                publishTo <<= version { v: String =>
                    val nexus = "https://oss.sonatype.org/"
                    if (v.trim.endsWith("SNAPSHOT"))
                        Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as ("maven") withPermissions ("0644"))
                    else
                        Some("releases" at nexus + "service/local/staging/deploy/maven2")
                },
                // */
                publishMavenStyle := true,
                publishArtifact in Test := false,
                pomIncludeRepository := { x => false },
                pomExtra := (
                    <url>http://github.com/fwbrasil/radon-stm/</url>
                    <licenses>
                        <license>
                            <name>LGPL</name>
                            <url>https://github.com/fwbrasil/radon-stm/blob/master/LICENSE-LGPL</url>
                            <distribution>repo</distribution>
                        </license>
                    </licenses>
                    <scm>
                        <url>git@github.com:fwbrasil/radon-stm.git</url>
                        <connection>scm:git:git@github.com:fwbrasil/radon-stm.git</connection>
                    </scm>
                    <developers>
                        <developer>
                            <id>fwbrasil</id>
                            <name>Flavio W. Brasil</name>
                            <url>http://fwbrasil.net</url>
                        </developer>
                    </developers>),
                organization := "net.fwbrasil"))

}