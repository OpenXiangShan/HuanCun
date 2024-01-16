/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package huancun

import chisel3._
import chisel3.util._

abstract class AssociativePolicy {
  val name: String
  val hashFuncList: List[UInt => UInt]
  val inverseFuncList: List[(UInt, UInt) => UInt]
  val hash_granularity: Int
  def choose_a_hash_func(way: UInt): UInt = way % hash_granularity.asUInt
  def get_hashed_index(lineAddr: UInt) = VecInit(hashFuncList.map(_(lineAddr)))
  def get_unhashed_index(tag: UInt, hash_value: Vec[UInt]) = VecInit(inverseFuncList.zipWithIndex.map {
    case (inv_func, id) => inv_func(tag, hash_value(id))
  })
}

object AssociativePolicy {
  def apply(s: String, setBits: Int, hash_granularity: Int = 1): AssociativePolicy = {
    val policy = s.toLowerCase() match {
      case "set"  => new SetAssociative(setBits)
      case "skew" => new SkewAssociative(setBits, hash_granularity)
      case t => throw new IllegalArgumentException(s"unknown Associative Policy type $t")
    }
    assert(policy.hashFuncList.size == policy.inverseFuncList.size, "The number of hash functions should be the same " +
    "as the number of inverse functions")
    policy
  }
}

class SetAssociative(setBits :Int) extends AssociativePolicy {
  val name = "set-associative"
  val hashFuncList = List(
    (lineAddr: UInt) => lineAddr(setBits - 1, 0)
  )
  val inverseFuncList = List(
    (tag: UInt, hash_value: UInt) => hash_value
  )
  val hash_granularity = hashFuncList.size
}

class SkewAssociative(setBits: Int, hashGranularity: Int) extends AssociativePolicy {
  object OptionalHashFunction{
  /** 
    * H and H_inv are inverse of each other
    * H:     [x(n), x(n - 1), ... , x(2), x(1)] ---> [x(n) ^ x(1), x(n), x(n - 1), ... , x(3), x(2)]
    * H_inv: [x(n), x(n - 1), ... , x(2), x(1)] ---> [x(n - 1), x(n - 2), ... , x(2), x(1), x(n) ^ x(n - 1)]
    */
    def H(x: UInt): UInt = {
      val bitWidth = x.getWidth
      if (bitWidth > 0) Cat(x(0) ^ x(bitWidth - 1), x(bitWidth - 1, 1)) else 0.U
    }
    def H_inv(x: UInt): UInt = {
      val bitWidth = x.getWidth
      if (bitWidth > 0) Cat(x(bitWidth - 2, 0), x(bitWidth - 1) ^ x(bitWidth - 2)) else 0.U
    }
  /** 
    * F and F_inv are inverse of each other
    * F:     [x(n), x(n - 1), ... , x(2), x(1)] ---> [x(1), x(n) ^ x(n - 1), x(n - 1) ^ x(n - 2), ... , x(2) ^ x(1)]
    * F_inv: [x(n), x(n - 1), ... , x(2), x(1)] ---> [x(n) ^ x(n - 1, 1).xorR, x(n) ^ x(n - 2, 1).xorR, ... , x(n) ^ x(1, 1).xorR, x(n)]
    */
    def F(x: UInt): UInt = H(x) ^ x
    def F_inv(x: UInt): UInt = {
      val bitWidth = x.getWidth
      if (bitWidth > 0) {
        val item = for (i <- 0 until (bitWidth - 1)) yield (x(bitWidth - 1) ^ x(i, 0).xorR).asUInt
        val bits = item.reverse.reduce(Cat(_, _))
        Cat(bits, x(bitWidth - 1))
      }
      else
        0.U
    }
  /**
    * G and G_inv are inverse of each other
    * G:     [x(n), x(n - 1), ... , x(2), x(1)] ---> [x(n) ^ x(n - 1), x(n - 1) ^ x(n - 2), ... , x(2) ^ x(1), x(n) ^ x(n - 1) ^ x(1)]
    * G_inv: [x(n), x(n - 1), ... , x(2), x(1)] ---> [x(n - 1, 1).xorR, x(n) ^ x(n - 1, 1).xorR, x(n) ^ x(n - 2, 1).xorR, ... , x(n) ^ x(1, 1).xorR]
    */ 
    def G(x: UInt): UInt = H_inv(x) ^ x
    def G_inv(x: UInt): UInt = {
      val bitWidth = x.getWidth
      if  (bitWidth > 0) {
        val item = for (i <- 0 until (bitWidth - 1)) yield (x(bitWidth - 1) ^ x(i, 0).xorR).asUInt
        val bits = item.reverse.reduce(Cat(_, _))
        Cat(x(bitWidth - 2, 0).xorR, bits)
      }
      else
        0.U
    }
    val hashFuncList = List(
      (lineAddr: UInt) => {
        val addrBits = lineAddr.getWidth
        val set = lineAddr(setBits - 1, 0)
        val tag = lineAddr(addrBits - 1, setBits)
        val useTagBits = tag.getWidth.min(setBits)
        H(set) ^ G(tag(useTagBits - 1, 0))
      },
      (lineAddr: UInt) => {
        val addrBits = lineAddr.getWidth
        val set = lineAddr(setBits - 1, 0)
        val tag = lineAddr(addrBits - 1, setBits)
        val useTagBits = tag.getWidth.min(setBits)
        H(tag(useTagBits - 1, 0)) ^ G(set)
      },
      (lineAddr: UInt) => {
        val addrBits = lineAddr.getWidth
        val set = lineAddr(setBits - 1, 0)
        val tag = lineAddr(addrBits - 1, setBits)
        val useTagBits = tag.getWidth.min(setBits)
        H_inv(set) ^ F(tag(useTagBits - 1, 0))
      },
      (lineAddr: UInt) => {
        val addrBits = lineAddr.getWidth
        val set = lineAddr(setBits - 1, 0)
        val tag = lineAddr(addrBits - 1, setBits)
        val useTagBits = tag.getWidth.min(setBits)
        H_inv(tag(useTagBits - 1, 0)) ^ F(set)
      }
    )
    val inverseFuncList = List(
      (tag: UInt, hash_value: UInt) => {
        val tagBits = tag.getWidth
        val useTagBits = tagBits.min(setBits)
        H_inv(hash_value ^ G(tag(useTagBits - 1, 0)))
      },
      (tag: UInt, hash_value: UInt) => {
        val tagBits = tag.getWidth
        val useTagBits = tagBits.min(setBits)
        G_inv(hash_value ^ H(tag(useTagBits - 1, 0)))
      },
      (tag: UInt, hash_value: UInt) => {
        val tagBits = tag.getWidth
        val useTagBits = tagBits.min(setBits)
        H(hash_value ^ F(tag(useTagBits - 1, 0)))
      },
      (tag: UInt, hash_value: UInt) => {
        val tagBits = tag.getWidth
        val useTagBits = tagBits.min(setBits)
        F_inv(hash_value ^ H_inv(tag(useTagBits - 1, 0)))
      }
    )
  }

  val name = "skew-associative"
  val hashFuncList = OptionalHashFunction.hashFuncList.take(hashGranularity)
  val inverseFuncList = OptionalHashFunction.inverseFuncList.take(hashGranularity)
  val hash_granularity: Int = hashFuncList.size
}
