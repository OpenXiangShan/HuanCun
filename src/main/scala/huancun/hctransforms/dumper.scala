package hctransforms

import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import firrtl.annotations.Annotation
import firrtl.transforms.{Flatten, FlattenAnnotation}

object Dumper {
  def dump(component: chisel3.Module): Unit = {
    println(s"Dumper: ${component.toNamed}")
    annotate(new ChiselAnnotation with RunFirrtlTransform {
      def toFirrtl: Annotation = FlattenAnnotation(component.toNamed)
      def transformClass = classOf[Flatten]
    })
  }
}
