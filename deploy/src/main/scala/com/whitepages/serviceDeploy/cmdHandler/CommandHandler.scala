package com.whitepages.serviceDeploy.cmdHandler

import akka.actor.ActorRefFactory
import com.whitepages.serviceDeploy.deploy.Requests.Progress
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
import Order._

case class CommandHandler(factory: ActorRefFactory, localHost: String, servicesInfo: ServicesInfo) {

  import servicesInfo._

  val deployState = CommandState(servicesInfo)

  import deployState._
  import PrintGrid._

  val reader = new ConsoleReader()
  val historyFile = new File(".history")
  if (!historyFile.exists()) {
    historyFile.createNewFile()
  }
  val history = new FileHistory(historyFile)
  reader.setHistory(history)


  private[this] var versions: Seq[String] = Seq[String]()

  private def getVersions(): Seq[String] = {
    versions = findServers match {
      case Some((serviceName, stackName, serverNames)) =>
        val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
        val allVers = coord.fromFirst("versions")
        val (host, vers) = allVers.head
        val v = jgetArray(vers, "versions").map(jgetString(_))
        val v1 = v.map {
          case v: String => (sortKey(v), v)
        }
        val v2 = v1.sortWith {
          case ((asort, a), (bsort, b)) =>
            asort > bsort
        }
        val versions = v2 map {
          case (asort, a) => a
        }
        coord.close
        versions
      case None => Seq[String]()
    }
    versions
  }

  def setCompleters {
    //lazy val versions = getVersions()
    reader.getCompleters foreach {
      case completer =>
        reader.removeCompleter(completer)
    }
    var completers = new util.LinkedList[Completer]()
    completers.add(new StringsCompleter("help"))
    completers.add(new StringsCompleter("quit"))
    completers.add(new StringsCompleter("services"))
    completers.add(new ArgumentCompleter(
      new StringsCompleter("service"),
      new StringsCompleter(seqAsJavaList(getServices))))
    service foreach {
      case serviceName =>
        completers.add(new StringsCompleter("stacks"))
        getStacks(serviceName) foreach {
          case stacks =>
            completers.add(new ArgumentCompleter(
              new StringsCompleter("stack"),
              new StringsCompleter(seqAsJavaList(stacks))))
            stack match {
              case Some(stackName) =>
                completers.add(new StringsCompleter("versions"))
                completers.add(new StringsCompleter("servers"))
                getServers(serviceName, stackName) match {
                  case Some(servers) =>
                    completers.add(new ArgumentCompleter(
                      new StringsCompleter("server"),
                      new StringsCompleter(seqAsJavaList(servers))))
                  case None =>
                }
                completers.add(new StringsCompleter("downloads"))
                //versions foreach {
                //case vers =>
                completers.add(new ArgumentCompleter(
                  new StringsCompleter("download"),
                  new StringsCompleter(versions)))
                //}
                completers.add(new StringsCompleter("status"))
                //versions foreach {
                //case vers =>
                completers.add(new ArgumentCompleter(
                  new StringsCompleter("deploy"),
                  new StringsCompleter(versions)))
                //}
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
    val path = service match {
      case Some(s1) =>
        "/" + s1 +
          (stack match {
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
    "deploy" + path + "> "
  }

  val cmds = JsonArray(
    JsonArray("COMMAND", "DESCRIPTION"),
    JsonArray("help", "list commands"),
    JsonArray("quit", "exit command loop"),
    JsonArray("services", "list services"),
    JsonArray("service name", "select service"),
    JsonArray("versions", "list of available service versions"),
    JsonArray("stacks", "list stacks of a service"),
    JsonArray("stack name", "select stack of a service"),
    JsonArray("servers", "list servers of a service stack"),
    JsonArray("server name", "select server of a service stack"),
    JsonArray("downloads", "list service versions on a stack or server"),
    JsonArray("download version", "download service version into stack or server"),
    JsonArray("status", "list status of service on a stack or server"),
    JsonArray("deploy version", "run service version in the containers on stack or server"),
    JsonArray("bounce", "bounce containers on stack or server"),
    JsonArray("halt", "stop running containers on stack or server")
  )

  def usage {
    printGrid(cmds)
  }

  def progress(p: Progress) {
    val color = if (p.level == "error") {
      Console.RED
    } else {
      Console.GREEN
    }
    println(s"   $color* ${p.host} ${p.msg}${Console.RESET}")
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
              //getVersions(factory, localHost, servicesInfo)
              stack = None
              server = None
              setCompleters
            case None => println("no such service: " + newService)

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
      } else if (cmd == "stacks") {
        findService match {
          case Some(s) =>
            (getStacks(s) match {
              case Some(ss) =>
                val grid = JsonArray("STACK") +:
                  (ss map {
                    case j => JsonArray(j)
                  })
                printGrid(grid)
              case None => println("no such service: " + s)
            })
          case None =>
        }
      } else if (cmd == "stack") {
        // TODO check new stack name valid
        if (args.size >= 2) {
          val newStack = args(1)
          findService match {
            case Some(serviceName) =>
              getStack(serviceName, newStack) match {
                case Some(j) =>
                  stack = Some(newStack)
                  server = None
                  if (versions.size == 0) getVersions()
                  setCompleters
                case None => println(s"no such stack $newStack")
              }
            case None =>
          }
        } else {
          println("no stack specified")
        }
      } else if (cmd == "servers") {
        findStack match {
          case Some((serviceName, stackName)) =>
            getServers(serviceName, stackName) match {
              case Some(ss) =>
                val grid = JsonArray("SERVERS") +:
                  (ss map {
                    case j => JsonArray(j)
                  })
                printGrid(grid)
              case None => println(s"no such server/stack $serviceName/$stackName")
            }
          case None =>
        }
      } else if (cmd == "server") {
        if (args.size >= 2) {
          val newServer = args(1)
          findStack match {
            case Some((serviceName, stackName)) =>
              getServer(serviceName, stackName, newServer) match {
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
        findServers match {
          case Some((serviceName, stackName, serverNames)) =>
            val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
            val info = coord.fromAll("downloads")
            coord.close
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
          case None =>
        }
      } else if (cmd == "download") {
        if (args.size >= 2) {
          val version = args(1)
          // TODO check version is legal??
          findServers match {
            case Some((serviceName, stackName, serverNames)) =>
              println("")
              val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
              coord.download(serviceName, version)
              coord.close
              println("")
            case None =>
          }
        } else {
          println("no version specified")
        }
      } else if (cmd == "status") {
        findServers match {
          case Some((serviceName, stackName, serverNames)) =>
            val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
            val result = coord.fromAll("status")
            coord.close
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
          case None =>
        }
      } else if (cmd == "deploy") {
        if (args.size >= 2) {
          val version = args(1)
          // TODO check version is legal??
          findServers match {
            case Some((serviceName, stackName, serverNames)) =>
              val (hasLB, select) = getStack(serviceName, stackName) match {
                case Some(j) =>
                  (jgetBoolean(j, "lb"),
                    jgetArray(j, "select").asInstanceOf[Seq[String]])
                case None => (false, Seq[String]())
              }
              println("")
              val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
              coord.deploy(serviceName, version, select, hasLB)
              coord.close
              println("")
            case None =>
          }
        } else {
          println("no version specified")
        }
      } else if (cmd == "halt") {
        findServers match {
          case Some((serviceName, stackName, serverNames)) =>
            println("")
            val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
            coord.halt()
            coord.close
            println("")
          case None =>
        }
      } else if (cmd == "bounce") {
        findServers match {
          case Some((serviceName, stackName, serverNames)) =>
            println("")
            val coord = Coordinator(factory, serviceName, localHost, serverNames, Some(progress))
            coord.bounce()
            coord.close
            println("")
          case None =>
        }
      } else {
        println(s"${Console.RED}unrecognized cmd: $cmd${Console.RESET}")
      }
    }
  }

  def run() {
    done = false
    while (!done) {
      reader.setPrompt(prompt)
      val line = reader.readLine()
      val args = line.split("\\s+")
      doCmd(args)
    }
  }

  def runCmds(cmds: Seq[String]): Unit = {
    for (cmd <- cmds) {
      val args = cmd.split("\\s+")
      println(s"${Console.BLUE}$prompt$cmd${Console.RESET}")
      doCmd(args)
    }
  }


}
