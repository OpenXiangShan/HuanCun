package huancun

import chipsalliance.rocketchip.config.Field
import freechips.rocketchip.tilelink.{TLChannelBeatBytes, TLEdgeIn, TLEdgeOut}
import freechips.rocketchip.util.{BundleFieldBase, BundleKeyBase}

case object CacheParamsKey extends Field[CacheParameters](CacheParameters())

case class CacheParameters(
  name:         String = "L2",
  level:        Int = 2,
  ways:         Int = 4,
  sets:         Int = 128,
  blockBytes:   Int = 64,
  replacement:  String = "plru",
  mshrs:        Int = 16,
  dirReadPorts: Int = 1,
  enableDebug:  Boolean = false,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  echoField:    Seq[BundleFieldBase] = Nil,
  reqField:     Seq[BundleFieldBase] = Nil, // master
  respKey:      Seq[BundleKeyBase] = Nil,
  reqKey:       Seq[BundleKeyBase] = Nil, // slave
  respField:    Seq[BundleFieldBase] = Nil) {
  require(ways > 0)
  require(sets > 0)
  require(channelBytes.d.get >= 8)
  require(dirReadPorts == 1, "now we only use 1 read port")
}

case object EdgeInKey extends Field[TLEdgeIn]
case object EdgeOutKey extends Field[TLEdgeOut]
