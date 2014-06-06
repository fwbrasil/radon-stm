import sbt._
import Keys._

object RadonStmBuild extends Build {

	/* Dependencies */
	val junit = "junit" % "junit" % "4.4" % "test"
	val specs2 = "org.specs2" %% "specs2" % "1.13" % "test"
	val scalaActors = "org.scala-lang" % "scala-actors" % "2.10.0" % "test"

	lazy val radonStm =
		Project(
			id = "radon-stm",
			base = file("."),
			settings = Defaults.defaultSettings ++ Seq(
				libraryDependencies ++=
					Seq(junit, specs2, scalaActors),
				javacOptions ++= Seq("-source", "1.5", "-target", "1.5"),
				// publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))), 
				// publishTo := Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as ("maven") withPermissions ("0644")),
				publishTo <<= version { v: String =>
				  val nexus = "https://oss.sonatype.org/"
				  if (v.trim.endsWith("SNAPSHOT")) 
				    Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as("maven") withPermissions("0644"))
				  else                             
				    Some("releases" at nexus + "service/local/staging/deploy/maven2")
				},
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
				  </developers>
				),
				organization := "net.fwbrasil",
				scalaVersion := "2.10.1",
				version := "1.5.2-SNAPSHOT"
			)
		)

}