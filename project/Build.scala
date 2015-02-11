import sbt._
import Keys._

object ScalazBuild extends Build {

  def testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.11.6" % "test")

  def scalazPrj(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scalaz-$prjName",
    libraryDependencies ++= testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2")
    )
  )

  // TODO Move to an other file
  val paradiseVersion = "2.1.0-M5"
  val paradisePlugin = compilerPlugin("org.scalamacros" %  "paradise" % paradiseVersion cross CrossVersion.full)

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).aggregate(prelude, core, clazz, io, rts, meta, examples)

  lazy val prelude = scalazPrj("prelude")
  lazy val core = scalazPrj("core").dependsOn(prelude, clazz)
  lazy val clazz = scalazPrj("class").dependsOn(prelude)

  lazy val io = scalazPrj("io")
  lazy val rts = scalazPrj("rts").dependsOn(core, io, core % "test->test")

  lazy val meta = scalazPrj("meta").dependsOn(clazz, core).settings(
    libraryDependencies ++= Seq(
      "org.scala-lang"  %  "scala-reflect"  % scalaVersion.value,
      "org.scala-lang"  %  "scala-compiler" % scalaVersion.value % "provided"
    ),
    addCompilerPlugin(paradisePlugin)
  )

  lazy val examples = scalazPrj("examples").dependsOn(core, rts, meta)
}
