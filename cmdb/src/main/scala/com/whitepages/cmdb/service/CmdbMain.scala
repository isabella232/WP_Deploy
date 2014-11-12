package com.whitepages.cmdb.service

import com.whitepages.framework.service.JsonService


class CmdbMain extends JsonService {
  val serviceName = "cmdb"
  val handlerFactory = CmdbHandlerFactory
  val monitorExt = None
  val queryStringHandler = None
}

object CmdbMain {

  def main(args: Array[String]) {
    val cmdb = new CmdbMain()
    cmdb.runServer()
  }

}
