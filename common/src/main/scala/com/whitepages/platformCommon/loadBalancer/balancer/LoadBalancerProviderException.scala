package com.whitepages.platformCommon.loadBalancer.balancer

sealed case class LoadBalancerProviderException(msg: String) extends Exception(msg)
