/** *************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2022 Peng Cheng Laboratory
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

package huancun.mbist

import chisel3._
import chisel3.util.experimental.BoringUtils

object EnableMBIST {
  def apply(): Boolean = true
}

object WiringUtils {
  def addSource(element: Data, name: String): Unit = {
    if (EnableMBIST()) {
      BoringUtils.addSource(element, name)
    }
  }

  def addSink(element: Data, name: String): Unit = {
    if (EnableMBIST()) {
      BoringUtils.addSink(element, name)
    }
  }
}

