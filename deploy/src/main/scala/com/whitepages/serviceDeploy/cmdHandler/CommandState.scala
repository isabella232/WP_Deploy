package com.whitepages.serviceDeploy.cmdHandler

import com.persist.JsonOps._

case class CommandState(info: ServicesInfo) {

  trait CmdState

  case object TopState extends CmdState

  case class ServiceState(service: String, group: String) extends CmdState

  case class EnvironmentState(environment: String) extends CmdState

  case class InstanceState(service: ServiceState, environment: EnvironmentState) extends CmdState

  // TODO remove after move to pc
  case class ServerState(service: ServiceState, environment: EnvironmentState, server: String) extends CmdState

  case class SubState(instance: InstanceState, sub: String) extends CmdState

  // TODO search, platform, data, ...
  case class GroupState(group: String) extends CmdState

  var cmdState: CmdState = TopState

  var manageMode: Boolean = false

  var password:String = ""

  def cmdGetService:String = {
    cmdState match {
      case s: ServiceState => s.service
      case i: InstanceState => i.service.service
      case s: ServerState => s.service.service
      case s: SubState => s.instance.service.service
      case x: Any => ""
    }
  }

  def prompt: String = {
    val path = if (cmdState == TopState) {
      ""
    } else {
      "/" + {
        cmdState match {
          case s: ServiceState => s"SVC:${s.service}"
          case e: EnvironmentState => s"ENV:${e.environment}"
          case i: InstanceState => s"${i.service.service}/${i.environment.environment}"
          case s: ServerState => s"${s.service.service}/${s.environment.environment}/${s.server}"
          case g: GroupState => s"GRP:${g.group}"
          case s: SubState => s"${s.instance.service.service}/${s.instance.environment.environment}/${s.sub}"
          case x: Any => "???"
        }
      }
    }
    s"${if (password.isEmpty)"" else "*"}${if (manageMode) Console.BLUE + "MANAGE@" + Console.RESET else ""}deploy${path}> "
  }

  def up: Unit = {
    cmdState = cmdState match {
      case s: ServiceState => TopState
      case e: EnvironmentState => TopState
      case i: InstanceState => i.service
      case g: GroupState => TopState
      case s: SubState => s.instance
      case x: Any => cmdState
    }

  }

  def properties: Json = {
    cmdState match {
      case TopState => emptyJsonObject
      case s: ServiceState =>
        info.getService(s.service) match {
          case Some(j) => j
          case None => emptyJsonObject
        }
      case e: EnvironmentState =>
        info.getEnvironment(e.environment) match {
          case Some(j) => j
          case None => emptyJsonObject
        }
      case i: InstanceState => {
        info.getEnv(i.service.service, i.environment.environment) match {
          case Some(j) => j
          case None => emptyJsonObject
        }
      }
      case s: ServerState => emptyJsonObject
      case g: GroupState => {
        emptyJsonObject
      }
      case s: SubState => {
        info.getSub(s.instance.service.service, s.instance.environment.environment, s.sub) match {
          case Some(j) => j
          case None => emptyJsonObject
        }
      }
      case x: Any => emptyJsonObject
    }

  }

  var service: Option[String] = None
  // TODO remove group after move to pc
  var group: Option[String] = None
  var env: Option[String] = None
  var server: Option[String] = None

  def findService: Option[(String, String)] = {
    (service, group) match {
      case (Some(serviceName), Some(groupName)) =>
        Some(serviceName, groupName)
      case x: Any =>
        println("no service specified")
        None
    }
  }

  def findEnv: Option[(String, String, String)] = {
    findService match {
      case Some((serviceName, groupName)) =>
        env match {
          case Some(envName) =>
            Some((serviceName, groupName, envName))
          case None =>
            println("no env specified")
            None
        }
      case None => None
    }
  }

  def findServer: Option[(String, String, String)] = {
    findEnv match {
      case Some((serviceName, groupName, envName)) =>
        server match {
          case Some(serverName) =>
            Some((serviceName, envName, serverName))
          case None =>
            println("no service specified")
            None
        }
      case None => None
    }
  }

  def findServers: Option[(String, String, String, Seq[String])] = {
    findEnv match {
      case Some((serviceName, groupName, envName)) =>
        server match {
          case Some(serverName) =>
            Some((serviceName, groupName, envName, Seq(serverName)))
          case None =>
            info.getServers(serviceName, envName) match {
              case Some(servers: Seq[String]) =>
                Some((serviceName, groupName, envName, servers))
              case None =>
                println("env not specified")
                None
            }
        }
      case None => None
    }

  }
}
