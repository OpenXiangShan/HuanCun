package huancun.utils

import firrtl.annotations.{Annotation, ModuleName, Named, SingleTargetAnnotation}
import chisel3._

case class SRAMClkDivBy2Annotation(mod: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  override val target: ModuleName = mod

  override def duplicate(n: ModuleName): Annotation = this.copy(n)
}

case class SRAMSpecialDepthAnnotation(mod: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  override val target: ModuleName = mod

  override def duplicate(n: ModuleName): Annotation = this.copy(n)
}

object CustomAnnotations {
  def annotateClkDivBy2(mod: Module) = {
    chisel3.experimental.annotate(mod)(Seq(SRAMClkDivBy2Annotation(mod.toNamed)))
  }
  def annotateSpecialDepth(mod: Module) = {
    chisel3.experimental.annotate(mod)(Seq(SRAMSpecialDepthAnnotation(mod.toNamed)))
  }
}
