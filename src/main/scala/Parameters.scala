package HuanCun

import chisel3._
import chisel3.util._

case class CacheParameters(
  // TODO: remove the following fixed parameters
  level:        Int = 2,
  ways:         Int = 4,
  sets:         Int,
  blockBytes:   Int = 64,
  beatBytes:    Int = 32,
  replacement:  String = "plru",
  uncachedGet:  Boolean = false,
  debug:        Boolean = false,
  enablePerf:   Boolean = false,
  verification: Boolean = false) // inner
{
  require(ways > 0)
  require(sets > 0)
  require(blockBytes > 0 && isPow2(blockBytes))
  require(beatBytes > 0 && isPow2(beatBytes))
  require(blockBytes >= beatBytes)

  val blocks = ways * sets
  val sizeBytes = blocks * blockBytes
  val blockBeats = blockBytes / beatBytes
}
