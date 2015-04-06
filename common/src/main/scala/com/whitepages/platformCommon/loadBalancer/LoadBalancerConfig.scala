package com.whitepages.platformCommon.loadBalancer

trait LoadBalancerConfig {
  val controlAddressKey = "controlAddress"
  val controlPortKey = "controlPort"
  val controlUserKey = "controlUser"
  val controlPasswordKey = "controlPassword"

  def nameFor(s: String, suffix: String) = s"platform-$s-$suffix"

}
