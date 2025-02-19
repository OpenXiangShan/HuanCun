package huancun.utils

import chisel3._
import chisel3.experimental.ChiselAnnotation

case class SRAMClkDivBy2Annotation(mod: firrtl.annotations.ModuleName)
  extends firrtl.annotations.SingleTargetAnnotation[firrtl.annotations.ModuleName] {
  override val target: firrtl.annotations.ModuleName = mod

  override def duplicate(n: firrtl.annotations.ModuleName): firrtl.annotations.Annotation = this.copy(n)
}

case class SRAMSpecialDepthAnnotation(mod: firrtl.annotations.ModuleName)
  extends firrtl.annotations.SingleTargetAnnotation[firrtl.annotations.ModuleName] {
  override val target: firrtl.annotations.ModuleName = mod

  override def duplicate(n: firrtl.annotations.ModuleName): firrtl.annotations.Annotation = this.copy(n)
}

object CustomAnnotations {
  def annotateClkDivBy2(mod: Module) = {
    chisel3.experimental.annotate(new ChiselAnnotation {
      override def toFirrtl: firrtl.annotations.Annotation = SRAMClkDivBy2Annotation(mod.toNamed)
    })
  }
  def annotateSpecialDepth(mod: Module) = {
    chisel3.experimental.annotate(new ChiselAnnotation {
      override def toFirrtl: firrtl.annotations.Annotation = SRAMSpecialDepthAnnotation(mod.toNamed)
    })
  }
}
