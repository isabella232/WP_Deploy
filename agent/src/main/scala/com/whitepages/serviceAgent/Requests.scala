package com.whitepages.serviceAgent

import scala.concurrent.Promise
import com.persist.JsonOps._

object Requests {

  case class LcInit(lb: Boolean, svcPort: Int)

  case class LcResult(expect:String = "")

  case class LcRun(p: Promise[LcResult], reportProgress: (Progress) => Unit, iname: String, select: Seq[String])

  case class LcStop(p: Promise[LcResult], reportProgress: (Progress) => Unit, iname: String)

  case class LcTimer(expect:String, id:Int)

  case class Progress(msg: String, level: String = "info", extra: JsonObject = emptyJsonObject)

}
