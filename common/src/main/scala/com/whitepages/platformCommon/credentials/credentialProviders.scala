package com.whitepages.platformCommon.credentials

import com.whitepages.platformCommon.crypto.{AllCredentials, Credentials}

object EnvironmentCredentialProvider extends CredentialProvider {

  def dnsCredentials(selectorOpt: Option[String]): CredentialPair = {
    CredentialPair(
      identity = getEnv("DNS_API_USER")
      , secret = getEnv("DNS_API_PASSWORD")
    )
  }

  def openstackCredentials(selectorOpt: Option[String]): CredentialPair = {
    CredentialPair(
      identity = getEnv("OS_USERNAME")
      , secret = getEnv("OS_PASSWORD")
    )
  }

  def lbCredentials(selectorOpt: Option[String]): CredentialPair = {

    def resolveEnvVariableInScope(scope: String, key: String): String = {
      getEnv(s"${scope.toUpperCase}_$key")
    }

    CredentialPair(
      identity = resolveEnvVariableInScope(selectorOpt.getOrElse(""), "LB_USER")
      , secret = resolveEnvVariableInScope(selectorOpt.getOrElse(""), "LB_PASSWORD")
    )
  }

  def awsCredentials(selectorOpt: Option[String]): CredentialPair = {
    CredentialPair(
      identity = getEnv("AWS_ACCESS_KEY")
      , secret = getEnv("AWS_SECRET_KEY")
    )
  }
}


object SecureCredentialProvider extends CredentialProvider {

  private lazy val (isSecure, allCredentials) = {
    val (isSecure, json) = Credentials.get()
    (isSecure, new AllCredentials(json))
  }

  private def signalError(msg: String) =
    throw new IllegalArgumentException(s"Credentials for $msg not found in ${if (isSecure) "secured" else "non-secured"} credential store")

  def dnsCredentials(selectorOpt: Option[String]): CredentialPair =
    allCredentials.dnsCredentials match {
      case Some(c) => c
      case None => signalError("DNS")
    }

  def openstackCredentials(selectorOpt: Option[String]): CredentialPair =
    allCredentials.openstackCredentials match {
      case Some(c) => c
      case None => signalError("Openstack")
    }

  def lbCredentials(selectorOpt: Option[String]): CredentialPair =
    allCredentials.lbCredentials(selectorOpt.getOrElse("")) match {
      case Some(c) => c
      case None => signalError("Load Balancer")
    }

  def awsCredentials(selectorOpt: Option[String]): CredentialPair =
    allCredentials.awsCredentials match {
      case Some(c) => c
      case None => signalError("AWS")
    }
}
