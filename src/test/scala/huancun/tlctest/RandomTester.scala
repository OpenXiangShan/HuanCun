package huancun.tlctest

import chiseltest._
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

    val totalTick = 50000
    val pendingThreshold = 6
    val nrAddingTrans = 16
    val rand = new Random(0xbeef)
    val addr_pool = {
      for (_ <- 0 to 128) yield (BigInt(rand.nextInt(0xffff) << 6) & BigInt(0xffff))
    }.distinct.toList // align to block size

    val addrRange = addr_pool.length
    val acquireProbMap = Map(branch -> 0.3, trunk -> 0.7)
    val releaseProbMap = Map(nothing -> 0.6, branch -> 0.3, trunk -> 0.1)

    var total_acquire = 0
    var total_release = 0
    var total_get = 0

    test(testTop.module).withAnnotations(testAnnos) { dut =>

      for (_ <- 0 to totalTick) {

        // Enqueue pending Acquire
        l1d_map(l1d => {
          if (l1d.outerAcquire.size <= pendingThreshold) {
            total_acquire += nrAddingTrans
            for (_ <- 0 until nrAddingTrans) {
              val addr = getRandomElement(addr_pool, rand)
              val targetPerm = sample(acquireProbMap, rand)
              l1d.addAcquire(addr, targetPerm)
            }
          }

          // Enqueue pending Release
          if (l1d.outerRelease.size <= pendingThreshold) {
            total_release += nrAddingTrans
            for (_ <- 0 until nrAddingTrans) {
              val addr = getRandomElement(addr_pool, rand)
              val targetPerm = sample(releaseProbMap, rand)
              l1d.addRelease(addr, targetPerm)
            }
          }
        })

        l1i_map(l1i => {
          if (l1i.outerGet.size <= pendingThreshold) {
            total_get += nrAddingTrans
            for (_ <- 0 until nrAddingTrans) {
              val addr = getRandomElement(addr_pool, rand)
              l1i.addGet(addr)
            }
          }
        })

        for((l1d, io) <- testTop.l1d_list.zip(dut.l1d_io)){
          l1d.update(io)
        }
        for((l1i, io) <- testTop.l1i_list.zip(dut.l1i_io)){
          l1i.update(io)
        }

        dut.clock.step(1)
      }

      println(s"total acquire: $total_acquire")
      println(s"total release: $total_release")
      println(s"total get: $total_get")
    }
  }
}
