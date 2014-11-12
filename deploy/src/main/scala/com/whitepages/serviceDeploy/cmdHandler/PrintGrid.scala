package com.whitepages.serviceDeploy.cmdHandler

import com.persist.JsonOps._
import scala.language.postfixOps

object PrintGrid {
  private def pad(s: String, size: Int): String = {
    if (s.size < size) {
      s + (" " * (size - s.size))
    } else {
      s
    }
  }

  def printGrid(j: JsonArray) {
    println("")
    val columns = jgetArray(j, 0).size
    val cid = Seq.range(0, columns)
    val maxSizes = cid map {
      case i =>
        val sizes = j map {
          case item =>
            jgetString(item, i).size
        }
        sizes.max
    }
    //println("ms=" + maxSizes)
    (j zipWithIndex) foreach {
      case (item, i) =>
        (jgetArray(item) zip maxSizes) foreach {
          case (v, size) =>
            print(pad(jgetString(v), size))
            print("  ")
        }
        println("")
    }
    println("")
  }
}
