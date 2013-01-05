import sbt._
import Keys._

object RadonStmBuild extends Build {

	/* Dependencies */
	val junit = "junit" % "junit" % "4.4" % "test"
	val specs2 = "org.specs2" %% "specs2" % "1.13" % "test"
	val scalaActors = "org.scala-lang" % "scala-actors" % "2.10.0" % "test"
	val scalaMeter = "com.github.axel22" %% "scalameter" % "0.2" % "test"

	/* Resolvers */
	val customResolvers = Seq(
		"snapshots" at "http://scala-tools.org/repo-snapshots",
		"releases" at "http://scala-tools.org/repo-releases",
		"Maven" at "http://repo1.maven.org/maven2/",
		"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
	)

	lazy val radonStm =
		Project(
			id = "radon-stm",
			base = file("."),
			settings = Defaults.defaultSettings ++ Seq(
				libraryDependencies ++=
					Seq(junit, specs2, scalaActors, scalaMeter),
				// publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))), 
				javacOptions ++= Seq("-source", "1.5", "-target", "1.5"),
				publishTo := Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as ("maven") withPermissions ("0644")),
				organization := "net.fwbrasil",
				scalaVersion := "2.10.0",
				version := "1.2-RC3",
				resolvers ++= customResolvers,
				testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
			)
		)

}