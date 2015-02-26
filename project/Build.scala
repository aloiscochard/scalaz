import sbt._
import Keys._

object ScalazBuild extends Build {

  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.11.6" % "test")

  val paradiseVersion = "2.1.0-M5"
  val paradisePlugin = compilerPlugin("org.scalamacros" %  "paradise" % paradiseVersion cross CrossVersion.full)

  def scalazPrj(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scalaz-$prjName",
    libraryDependencies ++= testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2")
    )
  )


  lazy val root = Project(
    id = "root",
    base = file(".")
  ).aggregate(prelude, core, clazz, io, rts, machines, meta, examples)

  lazy val prelude = scalazPrj("prelude")
  lazy val core = scalazPrj("core").dependsOn(prelude, clazz)
  lazy val clazz = scalazPrj("class").dependsOn(prelude)

  lazy val io = scalazPrj("io")
  lazy val rts = scalazPrj("rts").dependsOn(core, io, core % "test->test")

  lazy val machines = scalazPrj("machines").dependsOn(core, rts)

  lazy val meta = scalazPrj("meta").dependsOn(clazz, core).settings(
    libraryDependencies ++= Seq(
      "org.scala-lang"  %  "scala-reflect"  % scalaVersion.value,
      "org.scala-lang"  %  "scala-compiler" % scalaVersion.value % "provided"
    ),
    addCompilerPlugin(paradisePlugin)
  )

  lazy val examples = scalazPrj("examples").dependsOn(core, rts, meta, machines).settings(
    addCompilerPlugin(paradisePlugin)
  )
}
