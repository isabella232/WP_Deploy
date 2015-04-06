package com.whitepages.platformCommon.crypto

import com.persist.JsonOps._
import com.whitepages.framework.util.FileIO
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.reflect.io.File

object Credentials {

  protected val base = "/wp"

  private def read(file: String): String = {
    val f = FileIO.read(file)
    Await.result(f, 30 seconds)
  }

  private def write(file: String, v: String): Unit = {
    val f = FileIO.write(file, v)
    Await.result(f, 30 seconds)
  }

  def getSecure(priv: String): Json = {
    val sym = read(s"$base/secure/sym")
    val data = read(s"$base/secure/data")
    val s = Crypto.decrypt(Crypto.toPriv(priv), Crypto.Info(sym, data))
    Json(s)
  }

  def getInsecure(): Json = {
    val s = read(s"$base/insecure/data")
    Json(s)
  }

  // Boolean result isSecure or ! isLocal
  def get(): (Boolean, Json) = {
    try {
      (false, getInsecure())
    } catch {
      case ex: Throwable =>
        (true, getSecure(getPrivate()))
    }
  }

  def newKeys() {
    val keys = Crypto.getKeys()
    write(s"$base/insecure/public", Crypto.toS(keys.pub))
    write(s"$base/insecure/private", Crypto.toS(keys.priv))
  }

  def encrypt() {
    val f = File(s"$base/secure")
    f.createDirectory()
    val spub = read(s"$base/insecure/public")
    val pub = Crypto.toPub(spub)
    val data = read(s"$base/insecure/data")
    val info = Crypto.encrypt(pub, data)
    write(s"$base/secure/sym", info.symKey)
    write(s"$base/secure/data", info.value)
  }

  def getPrivate(): String = {
    try {
      read(s"$base/insecure/private")
    } catch {
      case ex: Throwable => System.getenv("WP_SECRET")
    }
  }

}
