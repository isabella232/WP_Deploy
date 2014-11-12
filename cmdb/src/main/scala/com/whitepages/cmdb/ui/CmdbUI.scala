package com.whitepages.cmdb.ui

import com.vaadin.server.VaadinRequest
import com.vaadin.ui._
import com.vaadin.annotations.Theme
import com.whitepages.cmdb.service.{CmdbHandlerFactory, CmdbMain}
import scala.concurrent.{Await, Promise, Future}
import com.whitepages.cmdb.graph.Neo4jOps
import com.persist.JsonOps._
import scala.concurrent.duration._
import scala.language.postfixOps

object StartRest {
  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  val ready = Promise[Neo4jOps]

  private def init1(ops1: Neo4jOps) {
    ready.success(ops1)
  }

  Future {
    CmdbHandlerFactory.uiInit = Some(init1)
    val cmdb = new CmdbMain()
    cmdb.runServer()
  }
}

@Theme("runo")
class CmdbUI extends UI {
  private var all: All = _

  def setact(c: Component): Unit = all.set(c)

  val uiUtil = UIUtil(setact)


  import uiUtil._

  private val ops = Await.result(StartRest.ready.future, 10 seconds)

  val uiDeploy = UIDeploy(ops, uiUtil)

  private def services: Component = {
    val info = ops.servicesSync
    data1("Services", info, service)
  }

  private def stacks: Component = {
    val info = ops.stacksSync
    data1("Stacks", info, stack)
  }

  private def people: Component = {
    val info = ops.peopleSync
    data1("People", info, person)
  }

  private def service(ref: String): Component = {
    val j = ops.serviceSync(ref)
    val name = jgetString(j, "service", "name")
    val id = jgetString(j, "service", "id")
    val versions = jgetArray(j, "versions")
    val clusters = jgetArray(j, "clusters") map {
      case ref: JsonObject =>
        val jc = ops.clusterSync(jgetString(ref, "id"))
        val stackName = jgetString(jc, "stack", "name")
        val cname = stackName + ":" + jgetString(ref, "name")
        ref + ("name" -> cname)
    }
    def act() {
      all.set(deployments("Service " + name, ops.serviceDeploymentsSync(id)))
    }

    panel("Service: " + name) {
      vertical()(
        button("All Deployments", act),
        ySpace(),
        item("Id", id),
        ySpace(),
        list1("Versions", versions, version),
        ySpace(),
        list1("Clusters", clusters, cluster)
      )
    }
  }

  private def stack(ref: String): Component = {
    val j = ops.stackSync(ref)
    def name = jgetString(j, "stack", "name")
    def id = jgetString(j, "stack", "id")
    val clusters = jgetArray(j, "clusters")
    def act() {
      all.set(deployments("Stack " + name, ops.stackDeploymentsSync(name)))
    }
    panel("Stack: " + name) {
      vertical()(
        horizontal()(
          button("All Deployments", act),
          xSpace(),
          button("All Events", () => ())
        ),
        ySpace(),
        item("Id", id),
        ySpace(),
        list1("Clusters", clusters, cluster)
      )
    }
  }

  private def person(ref: String): Component = {
    val j = ops.personSync(ref)
    val name = jgetString(j, "person", "name")
    val id = jgetString(j, "person", "id")
    def act() {
      all.set(deployments("Person " + name, ops.personDeploymentsSync(id)))
    }
    panel("Person: " + name) {
      vertical()(
        item("Id", id),
        ySpace(),
        button("All Deployments", act)
      )
    }
  }

  private def deploymentTree(clusterId: String): Component = {
    val j = ops.clusterSync(clusterId)
    val deployments = jgetArray(j, "deployments") filter {
      case v => jgetString(v, "t1") == ""
    }
    val scName = jget(j, "stack", "name") + ":" + jget(j, "cluster", "name") + " <= "
    val h = new HorizontalLayout()
    val vv: Component = if (deployments.size == 0) {
      new Label(scName + "NO CURRENT DEPLOYMENT")
    } else {
      val deploy = deployments.head
      val id = jgetString(deploy, "id")
      val d = ops.deploymentSync(id)
      val serviceName = jgetString(d, "service", "name")
      val versionName = jgetString(d, "version", "name")
      val dName = scName + serviceName + ":" + versionName
      val v = new VerticalLayout()
      val l = link1("", JsonObject("name" -> dName, "id" -> id), deployment)
      v.addComponent(l)
      val uses = jgetArray(d, "uses")
      for (use <- uses) {
        v.addComponent(deploymentTree(jgetString(use, "id")))
      }
      v
    }
    h.addComponent(xSpace(30))
    h.addComponent(vv)
    h
  }

  private def cluster(ref: String): Component = {
    val j = ops.clusterSync(ref)
    val clusterRef = jgetObject(j, "cluster")
    val name = jgetString(j, "cluster", "name")
    val id = jgetString(j, "cluster", "id")
    val stackRef = jgetObject(j, "stack")
    val stackName = jgetString(stackRef, "name")
    val serviceRef = jgetObject(j, "service")
    val deployments = jgetArray(j, "deployments")
    val servers = jgetArray(j, "servers")
    def reload() = all.set(cluster(ref))
    panel("Cluster: " + stackName + ":" + name) {
      vertical()(
        horizontal()(
          button("Deploy", UI.getCurrent.addWindow(uiDeploy.DeployWindow(stackRef, clusterRef, serviceRef, deployments, reload))),
          xSpace(),
          button("Un-Deploy", ()),
          xSpace(),
          button("Add Servers", ()),
          xSpace(),
          button("Remove Servers", ()),
          xSpace(),
          button("Change Dynamic", ())
        ),
        ySpace(),
        item("Id", id),
        ySpace(),
        link1("Stack", stackRef, stack),
        link1("Service", serviceRef, service),
        ySpace(),
        list1("Servers", servers, server),
        ySpace(),
        new Label("Current Deployments"),
        deploymentTree(ref)
      )
    }
  }

  private def version(ref: String): Component = {
    val j = ops.versionSync(ref)
    val schema = jgetObject(j, "version", "schema")
    val dynamic = jgetObject(j, "version", "dynamic")
    val name = jgetString(j, "version", "name")
    val id = jgetString(j, "version", "id")
    val serviceRef = jgetObject(j, "service")
    val serviceName = jgetString(serviceRef, "name")
    val currentDeployments = jgetArray(j, "deployments") filter {
      case v => jgetString(v, "t1") == ""
    } map {
      case v =>
        val id = jgetString(v, "id")
        val d = ops.deploymentSync(id)
        val stackName = jget(d, "stack", "name")
        val clusterName = jget(d, "cluster", "name")
        val dName = stackName + ":" + clusterName
        //println(Pretty(d))
        JsonObject("name" -> dName, "sort" -> jgetString(v, "sort"), "id" -> id)
    }
    val uses = jgetArray(j, "uses")
    def alld = {
      val allDeployments = jgetArray(j, "deployments")
      if (allDeployments.size == 0) {
        empty
      } else {
        button("All Deployments", all.set(deployments("Cluster " + name, allDeployments)))
      }
    }
    def dyn = {
      val v = new VerticalLayout()
      if (jsize(schema) > 0) {
        v addComponent (new Label("Dynamic Values"))
        for (((sn, sv), (dn, dv)) <- schema zip dynamic) yield {
          val t = sv match {
            case s: String => s
            case j: Any => Compact(j)
          }
          val s = s"   val $sn:$t=${Compact(dv)}"
          v.addComponent(new Label(s))
        }
        v.addComponent(ySpace())
      }
      v
    }
    panel("Version: " + serviceName + ":" + name) {
      vertical()(
        item("Id", id),
        ySpace(),
        link1("Service", serviceRef, service),
        ySpace(),
        list1("Uses", uses, service),
        ySpace(),
        dyn,
        // TODO expand to s:c <= s:v
        list1("Current Deployments", currentDeployments, deployment),
        ySpace(),
        alld
      )
    }
  }

  private def server(ref: String): Component = {
    val j = ops.serverSync(ref)
    val name = jgetString(j, "server", "name")
    val id = jgetString(j, "server", "id")
    val stackRef = jgetObject(j, "stack")
    val stackName = jgetString(stackRef, "name")
    val clusterRef = jgetObject(j, "cluster")
    val clusterName = jgetString(clusterRef, "name")
    panel("Server: " + stackName + ":" + clusterName + ":" + name) {
      vertical()(
        button("Restart", () => ()),
        ySpace(),
        item("Id", id),
        ySpace(),
        link1("Stack", stackRef, stack),
        link1("Cluster", clusterRef, cluster)
      )
    }
  }

  private def deployment(ref: String): Component = {
    val j = ops.deploymentSync(ref)
    val name = jgetString(j, "deployment", "name")
    val id = jgetString(j, "deployment", "id")
    val stackRef = jgetObject(j, "stack")
    val stackName = jgetString(stackRef, "name")
    val t0 = jgetString(j, "deployment", "t0")
    val t1 = jgetString(j, "deployment", "t1")
    val clusterRef = jgetObject(j, "cluster")
    val clusterName = jgetString(clusterRef, "name")
    val serviceRef = jgetObject(j, "service")
    val versionRef = jgetObject(j, "version")
    val personRef = jgetObject(j, "person")
    val uses = jgetArray(j, "uses") map {
      case use: JsonObject =>
        val clusterId = jgetString(use, "id")
        val clusterName = jgetString(use, "name")
        val j = ops.clusterSync(clusterId)
        val stackName = jgetString(j, "stack", "name")
        val name = stackName + ":" + clusterName
        JsonObject("name" -> name, "id" -> clusterId)
    }
    panel("Deployment: " + stackName + ":" + clusterName + ":" + name) {
      vertical()(
        item("Id", id),
        ySpace(),
        link1("Stack", stackRef, stack),
        link1("Cluster", clusterRef, cluster),
        ySpace(),
        link1("Service", serviceRef, service),
        link1("Version", versionRef, version),
        ySpace(),
        link1("Person", personRef, person),
        ySpace(),
        list1("Uses", uses, cluster),
        if (t0 == "" && t1 == "") empty else ySpace(),
        if (t0 == "") empty else item("T0", t0),
        if (t1 == "") empty else item("T1", t1)
      )
    }
  }

  private def deployments(title: String, refs: Seq[Json]): Component = {
    val t = new Table()
    t.setData()
    t.addContainerProperty("Person", classOf[SLink], null)
    t.addContainerProperty("Stack", classOf[SLink], null)
    t.addContainerProperty("Cluster", classOf[SLink], null)
    t.addContainerProperty("Service", classOf[SLink], null)
    t.addContainerProperty("Version", classOf[SLink], null)
    t.addContainerProperty("Deployment", classOf[SLink], null)
    t.addContainerProperty("T0", classOf[String], null)
    t.addContainerProperty("T1", classOf[String], null)
    t.setPageLength(refs.size)
    for ((ref, idx) <- refs.zipWithIndex) {
      val j = ops.deploymentSync(jgetString(ref, "id"))
      val personRef = jgetObject(j, "person")
      val personLink = slink(personRef, person)

      val stackRef = jgetObject(j, "stack")
      val stackLink = slink(stackRef, stack)

      val clusterRef = jgetObject(j, "cluster")
      val clusterLink = slink(clusterRef, cluster)

      val serviceRef = jgetObject(j, "service")
      val serviceLink = slink(serviceRef, service)

      val versionRef = jgetObject(j, "version")
      val versionLink = slink(versionRef, version)

      val deploymentRef = jgetObject(j, "deployment")
      val deploymentLink = slink(deploymentRef, deployment)

      val t0 = jgetString(j, "deployment", "t0")
      val t1a = jgetString(j, "deployment", "t1")
      val t1 = if (t1a == "") "CURRENT" else t1a
      t.addItem(Seq(personLink, stackLink, clusterLink, serviceLink, versionLink, deploymentLink, t0, t1).toArray, new Integer(idx + 1))
    }
    t.sort(Array("T1"), Array(false))
    panel(title + " Deployments")(
      t
    )
  }

  private def buttons() = {
    horizontal(BORDER)(
      button("Services", all.set(services)),
      xSpace(),
      button("Stacks", all.set(stacks)),
      xSpace(),
      button("People", all.set(people)),
      xSpace(),
      xSpace(),
      button("Set Time", ()),
      xSpace(),
      button("Login", ())
    )
  }

  private case class All() {
    val all = new VerticalLayout()
    all.setSizeFull()
    all.addComponent(buttons())
    private val content = new HorizontalLayout()
    content.setSizeFull()
    all.addComponent(content)
    all.setExpandRatio(content, 1.0f)

    def set(c: Component) {
      content.removeAllComponents()
      content.addComponent(c)
    }
  }

  protected def init(request: VaadinRequest) {
    all = All()
    setContent(all.all)
    all.set(services)
  }

}