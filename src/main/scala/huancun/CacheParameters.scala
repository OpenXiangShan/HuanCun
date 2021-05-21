package huancun

import chipsalliance.rocketchip.config.Field
import freechips.rocketchip.tilelink.TLChannelBeatBytes
import freechips.rocketchip.util.{BundleFieldBase, BundleKeyBase}

case object CacheParamsKey extends Field[CacheParameters](CacheParameters())

case class CacheParameters(
  name:         String = "L2",
  level:        Int = 2,
  ways:         Int = 4,
  sets:         Int = 1024,
  blockBytes:   Int = 64,
  replacement:  String = "plru",
  mshrs:        Int = 16,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  echoField:    Seq[BundleFieldBase] = Nil,
  reqField:     Seq[BundleFieldBase] = Nil, // master
  respKey:      Seq[BundleKeyBase] = Nil,
  reqKey:       Seq[BundleKeyBase] = Nil, // slave
  respField:    Seq[BundleFieldBase] = Nil) {
  require(ways > 0)
  require(sets > 0)
  require(channelBytes.d.get >= 8)
}
