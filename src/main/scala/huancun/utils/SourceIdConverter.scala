package huancun.utils

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{TLBundle, TLEdgeIn, TLXbar}

object SourceIdConverter {

  def convert(raw: Seq[(TLBundle, TLEdgeIn)]): Seq[(TLBundle, TLEdgeIn)] = {
    val (rawIn, rawEdges) = raw.unzip
    val inputIds = TLXbar.mapInputIds(rawEdges.map(_.client))
    val newEdges = rawEdges.zip(inputIds).map {
      case (edge, range) =>
        println(s"${edge.client.clients.map(_.name).mkString(" ")} -> ${range}")
        new TLEdgeIn(
          edge.client.v1copy(edge.client.clients.map(c => c.v1copy(sourceId = c.sourceId.shift(range.start)))),
          edge.manager,
          edge.params,
          edge.sourceInfo
        )
    }
    def cut(x: UInt, n: Int): UInt = x(log2Up(n) - 1, 0)
    val newBundles = newEdges.map(e => Wire(TLBundle(e.bundle)))
    for (((n, o), range) <- newBundles.zip(rawIn).zip(inputIds)) {
      n <> o
      n.a.bits.source := o.a.bits.source | range.start.U // o -> n
      o.b.bits.source := cut(n.b.bits.source, range.size) // o <- n
      n.c.bits.source := o.c.bits.source | range.start.U // o -> n
      o.d.bits.source := cut(n.d.bits.source, range.size) // o <- n
    }
    newBundles.zip(newEdges)
  }
}
