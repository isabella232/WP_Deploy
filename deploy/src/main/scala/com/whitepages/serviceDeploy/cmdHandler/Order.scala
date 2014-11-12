package com.whitepages.serviceDeploy.cmdHandler

import com.persist.JsonOps._

object Order {

  // v1.3.7-a  => ["v",1,".",3,".",7,"-a"] => key

  private def split(s: String): JsonArray = {
    var result = Seq[Json]()
    if (s.size > 0) {
      var name = ""
      var number = 0
      var isNumber = false
      for (i <- s) {
        if ('0' <= i && i <= '9') {
          val j = i.charValue() - '0'.charValue()
          if (!isNumber) {
            result = name +: result
            number = 0
            isNumber = true
          }
          number = number * 10 + j
        } else {
          if (isNumber) {
            result = number +: result
            name = ""
            isNumber = false
          }
          name = name + i
        }
      }
      if (isNumber) {
        result = number +: result
      } else {
        result = name +: result
      }
    }
    result.reverse
  }

  private val poss = "bcdefghijklmnopqrstuvwxy"
  private val negs = "YXWVUTSRQPONMLKJIHGFEDCB"

  private def comp(v: Long): Long = {
    var cover = 10
    while (v > cover) {
      cover = 10 * cover
    }
    (cover - v) + (cover / 10) - 1
  }

  private def keyEncode1(j: Json, last: Boolean): String = {
    j match {
      case s: String => {
        for (i <- 0 to s.length() - 1) {
          val ch = s(i)
          if (ch == '\u0000' || ch == '\uffff') {
            throw new Exception("illegal character in key string")
          }
        }
        // note last is an open prefix
        "*" + s + (if (last) {
          ""
        } else {
          "\u0000"
        })
      }
      case v: Long => {
        "#" +
          (if (v == Long.MinValue) {
            "A"
          } else if (v == Long.MaxValue) {
            "z"
          } else if (v < 0) {
            val v1 = comp(-v)
            val s = "" + v1
            val len = s.length()
            val pre = negs.substring(len - 1, len)
            pre + s
          } else if (v == 0) {
            "a"
          } else {
            // v > 0
            val s = "" + v
            val len = s.length()
            val pre = poss.substring(len - 1, len)
            pre + s
          })

      }
      case v: Int => keyEncode1(v.toLong, last)
      case a: JsonArray => {
        var result = "["
        var i = 0
        for (j1 <- a) {
          val s = keyEncode1(j1, false)
          result = result + s
          i += 1
        }
        if (!last) result = result + "]"
        result
      }
      case x => {
        throw new Exception("bad key form:" + x)
      }
    }
  }

  private def keyEncode(j: JsonArray): String = {
    keyEncode1(j, true)
  }

  def sortKey(name: String): String = keyEncode(split(name))

}
