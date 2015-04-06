package com.whitepages.platformCommon.crypto

import java.io.InputStream
import com.persist.JsonOps._
import com.whitepages.platformCommon.credentials.CredentialPair
import scala.io.Source

class AllCredentials(json: Json) {

  def this(ins: InputStream) = {
    this(scala.util.control.Exception.catching(classOf[Throwable]).opt {
      Json(Source.fromInputStream(ins, "UTF-8").getLines().mkString)
    }.getOrElse(emptyJsonObject))
  }

  def dnsCredentials = {
    if (jhas(json, "dns", "user") && jhas(json, "dns", "password"))
      Some(CredentialPair(jgetString(json, "dns", "user"), jgetString(json, "dns", "password")))
    else
      None
  }

  def awsCredentials = {
    if (jhas(json, "compute", "aws", "key") && jhas(json, "compute", "aws", "secret"))
      Some(CredentialPair(jgetString(json, "compute", "aws", "key"), jgetString(json, "compute", "aws", "secret")))
    else
      None
  }

  def openstackCredentials = {
    if (jhas(json, "compute", "openstack", "username") && jhas(json, "compute", "openstack", "password"))
      Some(CredentialPair(jgetString(json, "compute", "openstack", "username"), jgetString(json, "compute", "openstack", "password")))
    else
      None
  }

  def lbCredentials(env: String) = {
    if (jhas(json, "lb", "zeus", env, "user") && jhas(json, "lb", "zeus", env, "password"))
      Some(CredentialPair(jgetString(json, "lb", "zeus", env, "user"), jgetString(json, "lb", "zeus", env, "password")))
    else
      None
  }

}
