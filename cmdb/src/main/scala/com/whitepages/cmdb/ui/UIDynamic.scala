package com.whitepages.cmdb.ui

import com.vaadin.ui.{Button, NativeSelect, VerticalLayout, Window}
import com.persist.JsonOps._
import com.whitepages.cmdb.graph.Neo4jOps

case class UIDynamic(ops: Neo4jOps, util: UIUtil) {

  import util._

  /*
  private def deploy(info: JsonObject) {
    // TODO precondition checks
    // version and uses person all specified
    // not the same as the current version (newer?)
    // make sure uses has deployments

    val t0 = ops.util.now
    val deployments = jgetArray(info, "deployments").sortBy((j) => jgetString(j, "sort")).reverse
    val currentDeployments = deployments filter {
      case d => jgetString(d, "t1") == ""
    }
    //println(Pretty(info))
    //println(Pretty(currentDeployments))

    // Terminate and remove old deployment
    if (currentDeployments.size > 0) {
      val currentDeployment = currentDeployments.head
      val id = jgetString(currentDeployment, "id")
      //println("id=" + id)
      ops.undeploySync(id, t0)
    }

    val currentName = if (deployments.size > 0) {
      jgetString(deployments.head, "name")
    } else {
      "d0"
    }
    val currentIdx = try {
      currentName.substring(1).toInt
    } catch {
      case ex: Throwable => 0
    }
    val newName = "d" + (currentIdx + 1)

    //println("cn=" + currentName)
    //println("nn=" + newName)

    val clusterId = jgetString(info, "cluster", "id")
    val versionId = jgetString(info, "version", "id")
    val personId = jgetString(info, "person", "id")
    val useIds = jgetArray(info,"uses") map {
      case v => jgetString(v, "id")
    }

    // Create and link new deployment
    ops.deploySync(newName, t0, clusterId, versionId, personId, useIds)
  }
  */

  case class DynamicWindow(version: JsonObject, stackRef: JsonObject, clusterRef: JsonObject, serviceRef: JsonObject, deployments: JsonArray,
                          reload: () => Unit) extends Window("Dynamic") {
    //println(Compact(serviceRef) + ":" + ops)
    val clusterName = jgetString(clusterRef, "name")
    val serviceName = jgetString(serviceRef, "name")
    val stackName = jgetString(stackRef, "name")
    val versionName = jgetString(version, "name")
    setModal(true)
    setPositionX(200)
    setPositionY(200)
    val j = ops.serviceSync(jgetString(serviceRef, "id"))
    val verRefs = jgetArray(j, "versions") map {
      case j1 => jgetObject(j1)
    }
    val personRefs = ops.peopleSync

    //val uses = new VerticalLayout()

    var versionRef = emptyJsonObject
    var useSelects = Seq[NativeSelect]()

    /*
    def vact(versionRef1: JsonObject) {
      versionRef = versionRef1
      val versionId = jgetString(versionRef, "id")
      uses.removeAllComponents()
      val useRefs = if (versionRef == emptyJsonObject) {
        Seq()
      } else {
        val version = ops.versionSync(versionId)
        val schema = jgetObject(version, "version", "schema")
        if (jsize(schema) > 0) {
          uses.addComponent(new Button("Dynamic Properties"))
          uses.addComponent(ySpace())
        }

        jgetArray(version, "uses")
      }
      useSelects = for (useRef <- useRefs) yield {
        val useName = jgetString(useRef, "name")
        val useId = jgetString(useRef, "id")
        val j = ops.serviceSync(useId)
        var default = emptyJsonObject
        val clusterRefs = jgetArray(j, "clusters") map {
          case j1 =>
            val cref = jgetObject(j1)
            val cid = jgetString(j1, "id")
            val j2 = ops.clusterSync(cid)
            val sname = jgetString(j2, "stack", "name")
            if (sname == stackName) default = cref
            JsonObject("name" -> (sname + ":" + jgetString(cref, "name")), "id" -> jgetString(cref, "id"))
        }
        val c = select1("Uses " + useName + ". Select cluster", clusterRefs, (s: JsonObject) => (), default = default)
        uses.addComponent(c)
        uses.addComponent(ySpace())
        c
      }
    }

    val personSelect = select1("Deployed By", personRefs, (JsonObject) => ())

    def deployInfo = {
      val useRefs = useSelects map {
        case s =>
          val p = s.getValue.asInstanceOf[P2]
          JsonObject("name" -> p.name, "id" -> p.id)
      }
      val personInfo = personSelect.getValue.asInstanceOf[P2]
      val personRef = JsonObject("name" -> personInfo.name, "id" -> personInfo.id)

      JsonObject(
        "stack" -> stackRef,
        "cluster" -> clusterRef,
        "service" -> serviceRef,
        "version" -> versionRef,
        "person" -> personRef,
        "uses" -> useRefs,
        "deployments" -> deployments
      )
    }
    */

    def dact {
      // save new dynamic values
      this.close()
      reload()
    }

    val v = verticalBorder()(
      // stack:cluster <= service:version
      // values from version
      item("Stack", stackName),
      ySpace(),
      item("Cluster", clusterName),
      ySpace(),
      item("Service", serviceName),
      ySpace(),
      item("Version", versionName),
      ySpace(),
      button("Save", dact)
    )
    setContent(v)
  }

}
