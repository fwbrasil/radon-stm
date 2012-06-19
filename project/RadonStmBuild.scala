import sbt._
import Keys._

object RadonStmBuild extends Build {
  
	/* Dependencies */
  val junit = "junit" % "junit" % "4.4" % "test"
	val specs2 = "org.specs2" %% "specs2" % "1.9" % "test"
	val commonsCollections = "commons-collections" % "commons-collections" % "3.2.1"
	
	/* Resolvers */
	val customResolvers = Seq(
    "snapshots" at "http://scala-tools.org/repo-snapshots",
    "releases" at "http://scala-tools.org/repo-releases",
    "Maven" at "http://repo1.maven.org/maven2/"
	)
	
  lazy val radonStm = 
  	Project(
  		id = "radon-stm",
  		base = file("."),
  		settings = Defaults.defaultSettings ++ Seq(
        libraryDependencies ++= 
          Seq(junit, specs2, commonsCollections),
//      publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))), 
        publishTo := Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as("maven") withPermissions("0644")),
	      organization := "net.fwbrasil",
	      scalaVersion := "2.9.2",
        crossScalaVersions := Seq("2.9.1", "2.9.2"),
	      version := "1.0-RC1",
	      resolvers ++= customResolvers
      )
  	)

}