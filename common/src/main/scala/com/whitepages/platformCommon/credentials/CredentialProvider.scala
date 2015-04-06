package com.whitepages.platformCommon.credentials

trait CredentialProvider {
  protected def getEnv(key: String): String =
    Option(System.getenv(key)) match {
      case Some(v) => v
      case None => throw new IllegalArgumentException(s"Environment variable '$key' not set")
    }

  def dnsCredentials(selectorOpt: Option[String] = None): CredentialPair

  def lbCredentials(selectorOpt: Option[String] = None): CredentialPair

  def awsCredentials(selectorOpt: Option[String] = None): CredentialPair

  def openstackCredentials(selectorOpt: Option[String] = None): CredentialPair

}
