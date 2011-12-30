import sbt._
import Keys._

object RadonStmBuild extends Build {
  
	/* Dependencies */
  	val junit = "junit" % "junit" % "4.4" % "test"
	val specs2 = "org.specs2" %% "specs2" % "1.7" % "test"
	val scalaz = "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2" % "test"
	val commonsCollections = "commons-collections" % "commons-collections" % "3.2.1"
	
//	def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  	
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
    		settings = commonSettings ++ Seq(
              libraryDependencies ++= 
                  Seq(junit, specs2, scalaz, commonsCollections)
            )
    	)

    def commonSettings = 
    	Defaults.defaultSettings ++ Seq(
//    	    testFrameworks ++= Seq(specs2Framework),
    	    publishTo := Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as("maven") withPermissions("0644")),
    	    organization := "net.fwbrasil",
    	    scalaVersion := "2.9.1",
    	    version := "0.3-SNAPSHOT",
    	    resolvers ++= customResolvers
    	)
}