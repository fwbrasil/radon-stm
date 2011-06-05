import sbt._

class RadonStmProject(info: ProjectInfo) extends DefaultProject(info) {

	val junit = "junit" % "junit" % "4.4"
	val specs2 = "org.specs2" %% "specs2" % "1.3"
	val scalaz = "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2"

	def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
	override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

	val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
	val releases = "releases" at "http://scala-tools.org/repo-releases"

}