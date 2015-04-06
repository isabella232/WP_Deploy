package com.whitepages.serviceDeploy.cmdHandler

import akka.actor._
import scala.language.postfixOps
import java.net.NetworkInterface
import com.typesafe.config.{ConfigValue, ConfigValueFactory, ConfigFactory}
import scala.collection.JavaConversions._


object Deploy {

  def main(args: Array[String]): Unit = {
    try {
      main0(args)
      //System.exit(0)
    } catch {
      case ex: Throwable =>
        println("")
        println(Console.RED)
        println(s"Deploy failed: ${ex.getMessage}")
        ex.printStackTrace()
        println(Console.BLACK)
        println("")
        System.exit(1)
    }
  }

  def main0(args: Array[String]) {

    val optionArgs = args.filter(_.startsWith("-")).map {
      _.substring(1)
    }

    val options: Map[String, String] = (optionArgs.map {
      case s =>
        val parts = s.split("=")
        if (parts.size == 2) {
          (parts(0) -> parts(1))
        } else {
          (parts(0) -> "")
        }
    }).toMap

    val cmds = args.filter(!_.startsWith("-"))

    val net = NetworkInterface.getNetworkInterfaces().toSeq
    val net1 = net filter {
      case n =>
        n.getName.startsWith("en")
    }
    val all3 = net1 map {
      case n => n.getInetAddresses.toSeq
    }
    val all0 = all3.flatten
    val all = all0 filter {
      case n => n.isSiteLocalAddress
    }

    val all1 = all map {
      case item =>
        val parts = item.toString().split("/")
        parts(1)
    }
    // Hack to filter out vbox ip
    val all2 = all1 filter {
      case ip => !ip.startsWith("192.168.59.3")
    }
    val ip = all2.head
    println("ip= " + ip)

    val local = options.containsKey("local")
    val pc = options.containsKey("pc")

    val conf = ConfigFactory.load()
    var hostConfig: ConfigValue = ConfigValueFactory.fromAnyRef(ip, "local host")
    val conf1 = ConfigFactory.load().withValue("akka.remote.netty.tcp.hostname", hostConfig)
    val system = ActorSystem("deploy", conf1)

    val commandHandler = CommandHandler(system, ip, local, pc, ip)
    if (cmds.isEmpty) {
      commandHandler.run()

    } else {
      commandHandler.runCmds(cmds)
    }
    system.shutdown()
  }
}



