package com.whitepages.cmdb.graph

import org.joda.time.format.ISODateTimeFormat
import org.joda.time.DateTime
import scala.concurrent.Future
import com.persist.JsonOps._
import akka.actor.{ActorRef, ActorRefFactory}
import java.util.UUID
import com.whitepages.cmdb.util.Order
import com.whitepages.framework.util.{ClassSupport, Util}
import com.whitepages.framework.client.Neo4jClient

case class Neo4jUtil(actorFactory: ActorRefFactory) extends ClassSupport {

  val logFmt = ISODateTimeFormat.dateHourMinuteSecondFraction()

  def now = logFmt.print(DateTime.now())

  private[this] val client = Neo4jClient(actorFactory, "neo4j")

  private[this] implicit val ec = system.dispatcher

  def cypher(query: String): Future[Json] = {
    def request = JsonObject("query" -> query)
    //println(Pretty(request))
    client.postJson("db/data/cypher", request)
  }

  private def item(n: String, v: String) = {
    // TODO verify n is letter digit or -
    // verify v does not contain ' or escape
    s"$n:'$v'"
  }

  private def items(props: JsonObject) = {
    if (jsize(props) == 0) {
      ""
    } else {
      val x = for ((n, v) <- props) yield {
        val v1 = v match {
          case s: String => s
          case j => Compact(j)
        }
        item(n, v1)
      }
      x.mkString(" {", ",", "}")
    }
  }

  def nameId(j: JsonObject): JsonObject = {
    JsonObject("name" -> jgetString(j, "name"), "id" -> jgetString(j, "id"), "sort" -> jgetString(j, "sort"))
  }

  def cypherNode(label: String = "", name: String = "", props: JsonObject = emptyJsonObject) = {
    val label1 = if (label == "") "" else ":" + label
    s"($name$label1${items(props)})"
  }

  def cypherLeft(label: String, name: String = "", props: JsonObject = emptyJsonObject) = {
    s"<-[$name:$label${items(props)}]-"
  }

  def cypherRight(label: String, name: String = "", props: JsonObject = emptyJsonObject) = {
    s"-[$name:$label${items(props)}]->"
  }

  def cypherMatch(body: String, names: Seq[String]) = {
    cypher("match " + body + " return " + names.mkString(","))
  }

  private def cypherCreate(body: String, names: Seq[String]) = {
    cypher("create " + body + " return " + names.mkString(","))
  }


  def createNode(name: String, t0: String, label: String, props: JsonObject = emptyJsonObject,
                 id:String = UUID.randomUUID().toString()) = {
    val ni = JsonObject("name" -> name, "id" -> id, "sort" -> Order.sortKey(name), "t0" -> t0)
    val props1 = ni ++ props
    cypherCreate(cypherNode(label, name, props = props1), Seq(name))
  }

  def createRelationship(inId: String, inLabel: String, rel: String, outId: String, outLabel: String, props: JsonObject = emptyJsonObject,
                          id:String = UUID.randomUUID().toString()) = {
    val cmds = Seq(
      "match " + cypherNode(inLabel, "a", props = JsonObject("id" -> inId)),
      "match " + cypherNode(outLabel, "b", props = JsonObject("id" -> outId)),
      "create " + cypherNode(name = "a") + cypherRight(rel, "r", props = props ++ JsonObject("id" -> id)) +
        cypherNode(name = "b") + " return r"
    ).mkString(" ")
    cypher(cmds)
  }

  def stop() {
    client.stop
  }

  def isId(ref: String) = ref.length == 36 && ref.split("-").size == 5

  def props(ref: String) = {
    // TODO make sure id or name is valid
    if (isId(ref)) {
      JsonObject("id" -> ref)
    } else {
      JsonObject("name" -> ref)
    }
  }

  // labels.size == refLabels.size+1 == nodeNames.size
  def compoundRef(ref: String, labels: Seq[String], refLabels: Seq[String], nodeNames: Seq[String]): String = {
    val propsSeq = if (isId(ref)) {
      for ((label, i) <- labels.zipWithIndex) yield {
        if (i == labels.size - 1) {
          JsonObject("id" -> ref)
        } else {
          emptyJsonObject
        }
      }
    } else {
      // names.size == onames.size
      val names = ref.split(":").toSeq
      for (name <- names) yield {
        JsonObject("name" -> name)
      }
    }
    val rl = "" +: refLabels
    val items = for (((props, refLabel), (label, nodeName)) <- (propsSeq zip rl) zip (labels zip nodeNames)) yield {
      val ref = if (refLabel == "") {
        ""
      } else {
        cypherRight(refLabel)
      }
      ref + cypherNode(label, nodeName, props)
    }
    items.mkString("")
  }

}
