package huancun.tlctest

import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import huancun.{DumpVCD, UseVerilatorBackend}
import tltest._
import tltest.TLMessagesBigInt._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait RandomSampleUtil {
  def getRandomElement[A](l: List[A], r: scala.util.Random): A = {
    l(r.nextInt(l.length))
  }

  final def sample[A](dist: Map[A, Double], r: scala.util.Random): A = {
    val p = r.nextDouble
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen") // needed so it will compile
  }
}

class RandomTester extends TLCTest with RandomSampleUtil with DumpVCD with UseVerilatorBackend {
  it should "random run" in {

    val totalTick = 150000
    val pendingThreshold = 6
    val nrAddingTrans = 16
    val rand = new Random(0xbeef)
    val addr_pool = {
      for (_ <- 0 to 128) yield (BigInt(rand.nextInt(0xffff) << 6) & BigInt(0xffff))
    }.distinct.toList // align to block size

    val addrRange = addr_pool.length
    val acquireProbMap = Map(branch -> 0.3, trunk -> 0.7)
    val releaseProbMap = Map(nothing -> 0.6, branch -> 0.3, trunk -> 0.1)

    test(testTop.module).withAnnotations(testAnnos) { dut =>
      val l1d = testTop.l1d.agent

      for (_ <- 0 to totalTick) {

        // Enqueue pending Acquire
        if (l1d.outerAcquire.size <= pendingThreshold) {
          for (_ <- 0 until nrAddingTrans) {
            val addr = getRandomElement(addr_pool, rand)
            val targetPerm = sample(acquireProbMap, rand)
            l1d.addAcquire(addr, targetPerm)
          }
        }

        // Enqueue pending Release
        if (l1d.outerRelease.size <= pendingThreshold) {
          for (_ <- 0 until nrAddingTrans) {
            val addr = getRandomElement(addr_pool, rand)
            val targetPerm = sample(releaseProbMap, rand)
            l1d.addRelease(addr, targetPerm)
          }
        }

        l1d.issueA()
        l1d.issueC()
        testTop.l1d.update(dut.io)

        l1d.step()
        dut.clock.step(1)
      }
    }
  }
}
