package com.whitepages.platformCommon.loadBalancer.balancer

object ZeusDomainModel {

  sealed trait NodeState {
    def value: String
  }

  case object Active extends NodeState {
    val value = "active"
  }

  case object Disabled extends NodeState {
    val value = "disabled"
  }

  case object Draining extends NodeState {
    val value = "draining"
  }

  case class Node( node: String /* string in the form <host>:<port> */
                 , state: String=Disabled.value /* default: disabled */
                 , weight: Int=1 /* default for nodes added via the Web UI */
                 , priority: Int=1 /* default for nodes added via the Web UI */
                 , source_ip: Option[String]=None
                 ) {

    def port: Option[Int] = {
      val parts = node.split(':')
      if (parts.isEmpty) None
      else
        scala.util.control.Exception.catching(classOf[java.lang.NumberFormatException]).opt {
          parts.last.toInt
        }
    }

    def makeActive() = this.copy(state = Active.value)
    
    def makeDisabled() = this.copy(state = Disabled.value)
    
    def isDisabled = state == Disabled.value
    
    def isActive = state == Active.value
  }

}
