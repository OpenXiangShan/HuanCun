import mill._
import scalalib._
import scalafmt._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build

val defaultVersions = Map(
  "chisel" -> "5.0.0",
  "chisel-plugin" -> "5.0.0",
  "chiseltest" -> "5.0.0",
  "scala" -> "2.13.10",
  "scalatest" -> "3.2.7"
)

def getVersion(dep: String, org: String = "org.chipsalliance", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultVersions("scala")

  override def scalacPluginIvyDeps = Agg(getVersion("chisel-plugin", cross = true))

  override def scalacOptions = super.scalacOptions() ++ Agg("-Ymacro-annotations", "-Ytasty-reader")

}

object rocketchip extends `rocket-chip`.common.CommonRocketChip {

  val rcPath = os.pwd / "rocket-chip"

  override def scalaVersion = defaultVersions("scala")

  override def millSourcePath = rcPath

  object cdeRocket extends `rocket-chip`.cde.common.CDEModule with PublishModule {
    override def millSourcePath = rcPath / "cde" / "cde"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def pomSettings = T {
      rocketchip.pomSettings()
    }

    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      `rocket-chip`.common.getVersion("chisel3")
    ) else Agg.empty[Dep]
    
    def chisel3PluginIvyDeps = Agg(`rocket-chip`.common.getVersion("chisel3-plugin", cross=true))
  }

  def hardfloatModule = hardfloatRocket

  def cdeModule = cdeRocket

}


object utility extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(getVersion("chisel"))

  override def millSourcePath = os.pwd / "Utility"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
}


object HuanCun extends SbtModule with ScalafmtModule with CommonModule {

  override def millSourcePath = millOuterCtx.millSourcePath


  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel"),
    getVersion("chiseltest", "edu.berkeley.cs"),
  )

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip, utility)

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest","org.scalatest")
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework")

  /*
    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.tools.Runner", args: _*)
    }
  */
  }
}
