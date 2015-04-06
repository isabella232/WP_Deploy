package com.whitepages.serviceDeploy.cmdHandler

import akka.actor.ActorRefFactory
import com.whitepages.platformControllerClient.ControllerClient
import jline.console.ConsoleReader
import java.io.File
import jline.console.history.FileHistory
import java.util
import jline.console.completer.{AggregateCompleter, ArgumentCompleter, StringsCompleter, Completer}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import scala.collection.JavaConversions._
import com.persist.JsonOps._
import com.whitepages.serviceDeploy.deploy.Coordinator
import scala.language.postfixOps
import com.whitepages.platformUtil.Order.sortKey
import com.whitepages.platformControllerClient.ControllerClient._

case class ControllerClientWrapper(factory1: ActorRefFactory, localHost1: String, user1: String) {
  val cc = ControllerClient(factory1, localHost1, user1)

  var password: String = ""

  def callCond(cmd: String, options: JsonObject,
               progress: (Progress) => Unit,
               query: (String, Seq[String]) => String): Option[Json] = {
    val options1 = if (password == "") options else options ++ JsonObject("password" -> password)
    val r = cc.call(cmd, options1, progress, query)
    if (jgetBoolean(r, "fail")) {
      val msg = jgetString(r, "msg")
      println(s"   ${Console.RED}* $msg${Console.RESET}")
      None
    } else {
      Some(jget(r, "result"))
    }
  }

  def callObj(cmd: String, options: JsonObject,
              progress: (Progress) => Unit,
              query: (String, Seq[String]) => String): JsonObject = {
    callCond(cmd, options, progress, query) match {
      case Some(j) => jgetObject(j)
      case None => emptyJsonObject
    }
  }

  def callSeq(cmd: String, options: JsonObject,
              progress: (Progress) => Unit,
              query: (String, Seq[String]) => String): JsonArray = {
    callCond(cmd, options, progress, query) match {
      case Some(j) => jgetArray(j)
      case None => emptyJsonArray
    }
  }
}

case class CommandHandler(factory: ActorRefFactory, localHost: String, local: Boolean, pc: Boolean, ip: String) {
  private[this] val user = System.getProperty("user.name")
  private[this] val controllerClient = if (pc) {
    ControllerClientWrapper(factory: ActorRefFactory, localHost: String, user)
  } else {
    null
  }
  private[this] val servicesInfo = ServicesInfo(ip, local, pc, controllerClient)

  import servicesInfo._

  private[this] val deployState = CommandState(servicesInfo)

  import deployState._

  private[this] var versions: Seq[String] = Seq[String]()

  private def getVersions(): Seq[String] = {
    if (pc) {
      val serviceName = cmdGetService
      if (serviceName != "") {
        servicesInfo.getVersions(serviceName)
      } else {
        Seq.empty[String]
      }
    } else {
      findServers match {
        case Some((serviceName, groupName, envName, serverNames)) =>
          val varray = {
            //if (pc) {
            //val options = JsonObject("svc" -> serviceName)
            //val r = controllerClient.callSeq("ver", options, progress, query)
            //r
            //} else {
            val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
            val allVers = coord.fromFirst("versions")
            coord.close
            val (host, vers) = allVers.head
            jgetArray(vers, "versions")
          }
          val v = varray.map(jgetString(_))
          val v1 = v.map {
            case v: String => (sortKey(v), v)
          }
          val v2 = v1.sortWith {
            case ((asort, a), (bsort, b)) =>
              asort > bsort
          }
          versions = v2 map {
            case (asort, a) => a
          }
        //versions
        case None => Seq[String]()
      }

      versions
    }
  }

  import PrintGrid._

  private[this] val reader = new ConsoleReader()
  private[this] val historyFile = new File(".history")
  if (!historyFile.exists()) {
    historyFile.createNewFile()
  }
  private[this] val history = new FileHistory(historyFile)
  reader.setHistory(history)

  private def setQueryCompleters(vals: Seq[String]) {
    var old: Completer = null
    reader.getCompleters foreach {
      case completer =>
        old = completer
    }
    if (old != null) reader.removeCompleter(old)
    var completers = new util.LinkedList[Completer]()
    for (s <- vals) {
      completers.add(new StringsCompleter(s))
    }
    reader.addCompleter(new AggregateCompleter(completers))
  }

  private[this] var completers = new util.LinkedList[Completer]()

  private def resetCompleters: Unit = {
    var old: Completer = null
    reader.getCompleters foreach {
      case completer =>
        old = completer
    }
    if (old != null) reader.removeCompleter(old)
    reader.addCompleter(new AggregateCompleter(completers))
  }

  private def setCompleters {
    var old: Completer = null
    reader.getCompleters foreach {
      case completer =>
        old = completer
    }
    if (old != null) reader.removeCompleter(old)
    completers = new util.LinkedList[Completer]()
    completers.add(new StringsCompleter("help"))
    completers.add(new StringsCompleter("quit"))
    completers.add(new StringsCompleter("manage"))
    completers.add(new StringsCompleter("services"))
    if (pc) {
      completers.add(new StringsCompleter("top"))
      completers.add(new StringsCompleter("properties"))
      completers.add(new ArgumentCompleter(
        new StringsCompleter("environment"),
        new StringsCompleter(seqAsJavaList(getEnvironments()))))
      completers.add(new StringsCompleter("environments"))
    }
    if (pc && manageMode) {
      completers.add(new StringsCompleter("addService"))
      // TODO completers for list of envs
      completers.add(new StringsCompleter("addEnvironment"))
    }
    completers.add(new ArgumentCompleter(
      new StringsCompleter("service"),
      new StringsCompleter(seqAsJavaList(getServices))))
    service foreach {
      case serviceName =>
        if (!pc) completers.add(new StringsCompleter("environments"))
        if (pc) completers.add(new StringsCompleter("versions"))
        if (pc && manageMode) {
          completers.add(new StringsCompleter("addInstance"))
        }
        getEnvs(serviceName) foreach {
          case envs =>
            if (pc) completers.add(new StringsCompleter("instances"))
            completers.add(new ArgumentCompleter(
              new StringsCompleter(if (pc) "instance" else "environment"),
              new StringsCompleter(seqAsJavaList(envs))))
            env match {
              case Some(envName) =>
                if (!pc) {
                  completers.add(new StringsCompleter("versions"))
                  completers.add(new StringsCompleter("servers"))
                  getServers(serviceName, envName) match {
                    case Some(servers) =>
                      completers.add(new ArgumentCompleter(
                        new StringsCompleter("server"),
                        new StringsCompleter(seqAsJavaList(servers))))
                    case None =>
                  }
                }
                if (pc) {
                  completers.add(new StringsCompleter("subs"))
                  completers.add(new StringsCompleter("sub"))
                  completers.add(new StringsCompleter("addSub"))
                }
                completers.add(new StringsCompleter("downloads"))
                completers.add(new ArgumentCompleter(
                  new StringsCompleter("download"),
                  new StringsCompleter(versions)))
                completers.add(new StringsCompleter("status"))
                completers.add(new ArgumentCompleter(
                  new StringsCompleter("deploy"),
                  new StringsCompleter(versions)))
                if (manageMode) {
                  completers.add(new StringsCompleter("info"))
                  completers.add(new ArgumentCompleter(
                    new StringsCompleter("upgrade"),
                    new StringsCompleter(versions)))
                }
                completers.add(new StringsCompleter("bounce"))
                completers.add(new StringsCompleter("halt"))
              case None =>
            }
        }
    }
    completers.add(new ArgumentCompleter(
      new StringsCompleter("service"),
      new StringsCompleter(seqAsJavaList(getServices())))
    )
    reader.addCompleter(new AggregateCompleter(completers))
  }

  setCompleters

  def prompt: String = {
    if (pc) {
      deployState.prompt
    } else {

      val path = service match {
        case Some(s1) =>
          "/" + s1 +
            (env match {
              case Some(s2) =>
                "/" + s2 +
                  (server match {
                    case Some(s3) => "/" + s3
                    case None => ""
                  })
              case None => ""
            })
        case None => ""
      }
      s"${
        if (manageMode) Console.BLUE + "MANAGE@" + Console.RESET else ""
      }deploy" + path + "> "
    }
  }

  val pccmds = JsonArray(
    JsonArray("top", "goto top browse level"),
    JsonArray("up", "go up a browse level"),
    JsonArray("environments", "list environments"),
    JsonArray("environment name", "select environment")
  )
  val oldcmds = JsonArray(
    JsonArray("environments", "list environments of a service"),
    JsonArray("environment name", "select the environment for a service"),
    JsonArray("servers", "list servers of a service env"),
    JsonArray("server name", "select server of a service env")
  )

  val cmds = (if (pc) pccmds else oldcmds) ++
    JsonArray(
      JsonArray("COMMAND", "DESCRIPTION"),
      JsonArray("help", "list commands"),
      //JsonArray("top", "goto top browse level"),
      JsonArray("quit", "exit command loop"),
      JsonArray("services", "list services"),
      JsonArray("service name", "select service"),
      JsonArray("versions", "list of available service versions"),

      JsonArray("downloads", "list service versions on a env or server"),
      JsonArray("download version", "download service version into env or server"),
      JsonArray("status", "list status of service on a env or server"),
      JsonArray("deploy version", "run service version in the containers on env or server"),
      JsonArray("bounce", "bounce containers on env or server"),
      JsonArray("halt", "stop running containers on env or server"),
      JsonArray("manage", "enter/exit manage mode")
    ) ++ (if (manageMode) JsonArray(
    JsonArray("addService", "add a new service"),
    JsonArray("addEnvironment", "add a new environment"),
    JsonArray("addInstance", "add a new service instance (svc:env)"),
    JsonArray("info", "show CoreOS and base container info"),
    JsonArray("update", "update CoreOS and base container")
  )
  else emptyJsonArray)

  def usage {
    printGrid(cmds)
  }

  def progress(p: Progress) {
    val color = if (p.level == "error") {
      Console.RED
    } else {
      Console.GREEN
    }
    println(s"   $color* ${
      p.host
    } ${
      p.msg
    }${
      Console.RESET
    }")
  }

  def query(name: String, vals: Seq[String]): String = {
    setQueryCompleters(vals)
    reader.setHistoryEnabled(false)
    reader.setPrompt(s"${
      Console.CYAN
    }   * ??? $name = ${
      Console.RESET
    }")
    var arg = ""
    while (arg == "") {
      try {
      val line = reader.readLine()
      val args = line.split("\\s+")
      if (args.size > 0) arg = args(0)
      } catch {
        case ex:Throwable =>
          arg = "**cancel**"
      }
    }
    //println(arg)
    reader.setHistoryEnabled(true)
    resetCompleters
    arg
  }

  var done = false

  def doCmd(args: Array[String]) {
    if (args.size == 0 || (args.size == 1 && args(0) == "")) {
    } else {
      val cmd = args(0)
      if (cmd == "quit") {
        reader.getHistory().asInstanceOf[FileHistory].flush()
        //Deploy.p.success(()) // TODO do in shutdown hook
        done = true
      } else if (cmd == "manage") {
        manageMode = !manageMode
        setCompleters
      } else if (cmd == "help") {
        usage
      } else if (cmd == "services") {
        val grid = JsonArray("SERVICE") +:
          (getServices map {
            case j => JsonArray(j)
          })
        printGrid(grid)
      } else if (cmd == "service") {
        versions = Seq[String]()
        if (args.size >= 2) {
          val newService = args(1)
          getService(newService) match {
            case Some(s) =>
              service = Some(newService)
              val g = jgetString(s, "group")
              val g1 = if (g == "") "search" else g
              deployState.cmdState = deployState.ServiceState(newService, g1)
              //println(s"group:$g1")
              group = Some(g1)
              //getVersions(factory, localHost, servicesInfo)
              env = None
              server = None
              setCompleters
            case None => if (!pc) println("no such service: " + newService)

          }
        } else {
          println("no service specified")
        }
      } else if (cmd == "versions") {
        val versions = getVersions()
        setCompleters
        val lines = versions.map(JsonArray(_))
        val header = JsonArray(JsonArray("VERSION"))
        printGrid(header ++ lines)
      } else if (!pc && cmd == "environments") {
        findService match {
          case Some((serviceName, groupName)) =>
            (getEnvs(serviceName)) match {
              case Some(ss) =>
                val grid = JsonArray("ENV") +:
                  (ss map {
                    case j => JsonArray(j)
                  })
                printGrid(grid)
              case None => println("no such service: " + serviceName)
            }
          case None =>
        }
      } else if ((pc && cmd == "instance") || (!pc && cmd == "environment")) {
        // TODO context dependent: top and svc
        // TODO verify instance exists
        if (args.size >= 2) {
          val newEnv = args(1)
          cmdState match {
            case s: ServiceState => cmdState = InstanceState(s, EnvironmentState(newEnv))
            case x: Any =>
          }
          findService match {
            case Some((serviceName, groupName)) =>
              getEnv(serviceName, newEnv) match {
                case Some(j) =>
                  env = Some(newEnv)
                  server = None
                  if (versions.size == 0) getVersions()
                  setCompleters
                case None => println(s"no such env $newEnv")
              }
            case None =>
          }
        } else {
          println("no env specified")
        }
      } else if (pc && cmd == "sub") {
        if (args.size >= 2) {
          val sub = args(1)
          cmdState match {
            case i: InstanceState =>
              cmdState = SubState(i, sub)
              setCompleters
            case x: Any =>
          }
        } else {
          println("no env specified")
        }
      } else if (!pc && cmd == "servers") {
        findEnv match {
          case Some((serviceName, groupName, envName)) =>
            getServers(serviceName, envName) match {
              case Some(ss) =>
                val grid = JsonArray("SERVERS") +:
                  (ss map {
                    case j => JsonArray(j)
                  })
                printGrid(grid)
              case None => println(s"no such server/env $serviceName/$envName")
            }
          case None =>
        }
      } else if (!pc && cmd == "server") {
        if (args.size >= 2) {
          val newServer = args(1)
          findEnv match {
            case Some((serviceName, groupName, envName)) =>
              getServer(serviceName, envName, newServer) match {
                case Some(s) =>
                  server = Some(newServer)
                  setCompleters
                case None => println("no such server: " + newServer)
              }
            case None =>
          }
        } else {
          println("no server specified")
        }
      } else if (cmd == "downloads") {
        val info = if (pc) {
          cmdState match {
            case s: SubState =>
              val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                "sub" -> s.sub)
              val r = controllerClient.callObj("downloads", options, progress, query)
              //println("pc=" + Pretty(result))
              r
            case x: Any => emptyJsonObject
          }
        } else {
          findServers match {
            case Some((serviceName, groupName, envName, serverNames)) =>
              val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
              val info = coord.fromAll("downloads")
              coord.close
              info
            case None => emptyJsonObject
          }
        }
        val report = info flatMap {
          case (host, downloads) =>
            jgetArray(downloads, "images") map {
              case download =>
                val created0 = jgetLong(download, "created")
                val t0 = new DateTime(created0)
                val logFmt = ISODateTimeFormat.dateTimeNoMillis()
                val t = logFmt.print(t0)
                val created = logFmt.print(t0)
                // print as MB
                val mb = (jgetInt(download, "size") * 1.0 / 1000000.0)
                val size = "%1$.1f".format(mb) + " MB"
                JsonArray(host, jgetString(download, "version"), size, created)
            }
        }
        val header = JsonArray(JsonArray("SERVER", "VERSION", "SIZE", "CREATED"))
        printGrid(header ++ report.toSeq)
      } else if (cmd == "download") {
        if (args.size >= 2) {
          val version = args(1)
          // TODO check version is legal??
          findServers match {
            case Some((serviceName, groupName, envName, serverNames)) =>
              println("")
              if (pc) {
                cmdState match {
                  case s: SubState =>
                    val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                      "sub" -> s.sub, "ver" -> version)
                    val r = controllerClient.callObj("download", options, progress, query)
                  //println("pc=" + Pretty(r))
                  case x: Any => emptyJsonObject
                }
              } else {
                val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
                coord.download(serviceName, version)
                coord.close
              }
              println("")
            case None =>
          }
        } else {
          println("no version specified")
        }
      } else if (cmd == "info" && manageMode) {
        findServers match {
          case Some((serviceName, groupName, envName, serverNames)) =>
            val result = if (pc) {
              cmdState match {
                case s: SubState =>
                  val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                    "sub" -> s.sub)
                  val r = controllerClient.callObj("info", options, progress, query)
                  //println("pc=" + Pretty(result))
                  r
                case x: Any => emptyJsonObject
              }
            } else {
              val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
              val result = coord.fromAll("info")
              coord.close
              result
            }
            val report = result.map {
              case (ip, info) => JsonArray(ip
                , jgetString(info, "coreos")
                , jgetString(info, "agent", "version"), jgetString(info, "agent", "status")
                , jgetString(info, "tools", "version"), jgetString(info, "tools", "status")
                , jgetString(info, "logger", "version"), jgetString(info, "logger", "status")
              )
            }
            //println(Pretty(result))
            val header = JsonArray(JsonArray("SERVER", "COREOS", "AGENT VERSION", "AGENT STATUS", "TOOLS VERSION", "TOOLS STATUS", "LOGGER VERSION", "LOGGER STATUS"))
            printGrid(header ++ report)
          case None => /* nop */
        }


      } else if (cmd == "upgrade" && manageMode) {
        println("Upgrade NYI")
      } else if (cmd == "status") {
        findServers match {
          case Some((serviceName, groupName, envName, serverNames)) =>
            val result = if (pc) {
              cmdState match {
                case s: SubState =>
                  val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                    "sub" -> s.sub)
                  val r = controllerClient.callObj("status", options, progress, query)
                  //println("pc=" + Pretty(result))
                  r
                case x: Any => emptyJsonObject
              }
            } else {
              val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
              val result = coord.fromAll("status")
              coord.close
              //println("agent=" + Pretty(result))
              result

            }
            val report = result map {
              case (n, v) =>
                val ports = jgetArray(v, "ports") map {
                  case p => jgetInt(p).toString
                }
                val portList = if (ports.size == 0) "" else ports.mkString("[", ",", "]")
                JsonArray(n, jgetString(v, "version"), portList, jgetString(v, "status"),
                  jgetString(v, "lb"), jgetString(v, "agent"))
            }
            val header = JsonArray(JsonArray("SERVER", "VERSION", "PORTS", "STATUS", "LB", "AGENT"))
            printGrid(header ++ report)
          case None => /* nop */
        }
      } else if (cmd == "deploy") {
        if (args.size >= 2) {
          val version = args(1)
          // TODO check version is legal??
          findServers match {
            case Some((serviceName, groupName, envName, serverNames)) =>
              println("")
              if (pc) {
                cmdState match {
                  case s: SubState =>
                    val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                      "sub" -> s.sub, "ver" -> version)
                    val r = controllerClient.callObj("deploy", options, progress, query)
                  //println("pc=" + Pretty(r))
                  case x: Any => emptyJsonObject
                }
              } else {
                val (hasLB, select) = getEnv(serviceName, envName) match {
                  case Some(j) =>
                    (jgetBoolean(j, "hasLB"),
                      jgetArray(j, "select").asInstanceOf[Seq[String]])
                  case None => (false, Seq[String]())
                }
                val domain = getDomain(serviceName, envName)
                val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
                coord.deploy(serviceName, version, select, hasLB, domain.get)
                coord.close
              }
              println("")
            case None =>
          }
        } else {
          println("no version specified")
        }
      } else if (cmd == "addService" && pc && manageMode) {
        if (args.size >= 2) {
          val svc = args(1)
          val options = JsonObject("svc" -> svc)
          val r = controllerClient.callObj("addSvc", options, progress, query)
          setCompleters
        } else {
          println("no service specified")
        }
      } else if (cmd == "addEnvironment" && pc && manageMode) {
        if (args.size >= 2) {
          val env = args(1)
          val options = JsonObject("env" -> env)
          val r = controllerClient.callObj("addEnv", options, progress, query)
          setCompleters
        } else {
          println("no environment specified")
        }
      } else if (cmd == "addInstance" && pc && manageMode) {
        // TODO given svc spec env; given env spec svc
        if (args.size >= 2) {
          val environment = args(1)
          findService match {
            case Some((service, group)) =>
              val options = JsonObject("env" -> environment, "svc" -> service)
              val r = controllerClient.callObj("addIns", options, progress, query)
            case None =>
          }
        } else {
          println("no environment specified")
        }
      } else if (cmd == "addSub" && pc && manageMode) {
        if (args.size >= 2) {
          val sub = args(1)
          cmdState match {
            case i: InstanceState =>
              val options = JsonObject("env" -> i.environment.environment, "svc" -> i.service.service, "sub" -> sub)
              val r = controllerClient.callObj("addSub", options, progress, query)
            case x: Any =>
          }
        } else {
          println("no sub specified")
        }
      } else if (cmd == "properties" && pc) {
        // print properties (as Json??? or table???)
        val p = jgetObject(properties)
        if (jsize(p) > 0) {
          println("")
          for ((n, v) <- jgetObject(properties)) {
            println(s"   ${Console.GREEN}$n: ${Compact(v)}${Console.RESET}")
          }
          println("")
        }
        //println(Pretty(j))
      } else if (pc && cmd == "top") {
        cmdState = TopState
        service = None
        group = None
        env = None
        server = None
      } else if (pc && cmd == "up") {
        up
      } else if (cmd == "password" && pc) {
        // Authenticate special commands
        if (args.size >= 2) {
          val pw = args(1)
          controllerClient.password = pw
          password = pw
        } else {
          controllerClient.password = ""
          password = ""
        }
      } else if (pc && cmd == "environment") {
        if (args.size >= 2) {
          val newEnvironment = args(1)
          getEnvironment(newEnvironment) match {
            case Some(s) =>
              cmdState = EnvironmentState(newEnvironment)
              setCompleters
            case None =>
          }
        } else {
          println("environment not specified")
        }
      } else if (pc && cmd == "environments") {
        val grid = JsonArray("ENVIRONMENT") +:
          (getEnvironments map {
            case j => JsonArray(j)
          })
        printGrid(grid)
      } else if (pc && cmd == "instances") {
        val grid = JsonArray("INSTANCES") +: {
          cmdState match {
            case ServiceState(serviceName, group) =>
              (getInstances(serviceName) map {
                case j => JsonArray(j)
              })
            case x: Any => emptyJsonArray
          }
        }
        // TODO print svc:env
        printGrid(grid)
      } else if (pc && cmd == "subs") {
        val grid = JsonArray("SUBS") +: {
          cmdState match {
            case InstanceState(service, environment) =>
              (getSubs(service.service, environment.environment) map {
                case j => JsonArray(j)
              })
            case x: Any => emptyJsonArray
          }
        }
        printGrid(grid)
      } else if (cmd == "provision") {
        if (args.size >= 2) {
          println("provision accepts no arguments")
        } else {
          findServers match {
            case Some((serviceName, groupName, envName, serverNames)) =>
              val (minServers, maxServers) = getMinMaxServers(serviceName, envName)
              val (provider, region, zone) = getProviderRegionZone(serviceName, envName)
              //val provider = getDefaultProvider(serviceName, envName)
              val servers = getServers(serviceName, envName)
              val instanceType = getDefaultInstance(serviceName, envName)
              val loadBalancerPort = getLoadBalancerPort(serviceName, envName)
              val domain = getDomain(serviceName, envName)
              if (maxServers != None && servers.size >= maxServers.get) {
                println(s"$serviceName env $envName already has max servers ${
                  servers.size
                }")
              } else {
                println("")
                val numToProvision = if (maxServers == None) 1 else maxServers.get - servers.size
                val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
                val result = coord.provision(serviceName, envName, provider.get, region.get, zone.get, instanceType.get, numToProvision, loadBalancerPort.get, domain.get)
                coord.close
                println(Pretty(result))
              }
              println("")
            case None =>
          }
        }
      } else if (cmd == "provisionMaxServers") {
        if (args.size >= 2) {
          println("provisionMaxServers accepts no arguments")
        } else {
          findServers match {
            case Some((serviceName, groupName, envName, serverNames)) =>
              val (minServers, maxServers) = getMinMaxServers(serviceName, envName)
              val (provider, region, zone) = getProviderRegionZone(serviceName, envName)
              //val provider = getDefaultProvider(serviceName, envName)
              val servers = getServers(serviceName, envName)
              val instanceType = getDefaultInstance(serviceName, envName)
              if (maxServers != None && servers.size >= maxServers.get) {
                println(s"$serviceName env $envName already has max servers ${
                  servers.size
                }")
              } else {
                println("")
                val numToProvision = if (maxServers == None) 1 else maxServers.get - servers.size
                val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
                val result = coord.provisionServers(serviceName, envName, provider.get, region.get, zone.get, instanceType.get, numToProvision)
                coord.close
                println(Pretty(result))
              }
              println("")
            case None =>
          }
        }
      } else if (cmd == "halt") {
        findServers match {
          case Some((serviceName, groupName, envName, serverNames)) =>
            println("")
            if (pc) {
              cmdState match {
                case s: SubState =>
                  val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                    "sub" -> s.sub)
                  val r = controllerClient.callObj("halt", options, progress, query)
                //println("pc=" + Pretty(r))
                case x: Any => emptyJsonObject
              }
            } else {
              val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
              coord.halt()
              coord.close
            }
            println("")
          case None =>
        }
      } else if (cmd == "bounce") {
        findServers match {
          case Some((serviceName, groupName, envName, serverNames)) =>
            println("")
            if (pc) {
              cmdState match {
                case s: SubState =>
                  val options = JsonObject("svc" -> s.instance.service.service, "env" -> s.instance.environment.environment,
                    "sub" -> s.sub)
                  val r = controllerClient.callObj("bounce", options, progress, query)
                //println("pc=" + Pretty(r))
                case x: Any => emptyJsonObject
              }
            } else {
              val coord = Coordinator(factory, serviceName, groupName, envName, localHost, serverNames, Some(progress))
              coord.bounce()
              coord.close
            }
            println("")
          case None =>
        }
      } else {
        println(s"${
          Console.RED
        }unrecognized cmd: $cmd${
          Console.RESET
        }")
      }
    }
  }

  def run() {
    done = false
    reader.setHandleUserInterrupt(true)
    while (!done) {
      reader.setPrompt(prompt)
      try {
        val line = reader.readLine()
        val args = line.split("\\s+")
        doCmd(args)
      } catch {
        case ex:Throwable =>
          //println(ex.toString)
      }
    }
  }

  def runCmds(cmds: Seq[String]): Unit = {
    for (cmd <- cmds) {
      val args = cmd.split("\\s+")
      println(s"${
        Console.BLUE
      }$prompt$cmd${
        Console.RESET
      }")
      doCmd(args)
    }
  }


}
