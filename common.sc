import mill._
import scalalib._

trait HuanCunModule extends ScalaModule {

  def rocketModule: ScalaModule

  def utilityModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(rocketModule, utilityModule)

}
