package com.whitepages.serviceDeploy.cmdHandler

case class CommandState(info: ServicesInfo) {
  var service: Option[String] = None
  var stack: Option[String] = None
  var server: Option[String] = None

  def findService: Option[String] = {
    service match {
      case Some(serviceName) =>
        Some(serviceName)
      case None =>
        println("no service specified")
        None
    }
  }

  def findStack: Option[(String, String)] = {
    findService match {
      case Some(serviceName) =>
        stack match {
          case Some(stackName) =>
            Some((serviceName, stackName))
          case None =>
            println("no stack specified")
            None
        }
      case None => None
    }
  }

  def findServer: Option[(String, String, String)] = {
    findStack match {
      case Some((serviceName, stackName)) =>
        server match {
          case Some(serverName) =>
            Some((serviceName, stackName, serverName))
          case None =>
            println("no service specified")
            None
        }
      case None => None
    }
  }

  def findServers: Option[(String, String, Seq[String])] = {
    findStack match {
      case Some((serviceName, stackName)) =>
        server match {
          case Some(serverName) =>
            Some((serviceName, stackName, Seq(serverName)))
          case None =>
            info.getServers(serviceName, stackName) match {
              case Some(servers: Seq[String]) =>
                Some((serviceName, stackName, servers))
              case None =>
                println("stack not specified")
                None
            }
        }
      case None => None
    }

  }
}
