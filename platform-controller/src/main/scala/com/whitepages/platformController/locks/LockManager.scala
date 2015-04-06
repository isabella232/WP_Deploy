package com.whitepages.platformController.locks

import akka.actor.{Props, Cancellable}
import com.whitepages.framework.exceptions.{NotAvailableException, BadInputException}
import com.whitepages.framework.logging.noId
import com.whitepages.framework.util.{ClassSupport, CheckedActor}
import scala.concurrent.{ExecutionContext, Await, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.language.postfixOps
import com.persist.JsonOps._

case class LockManager() extends ClassSupport {
  val lockActor = system.actorOf(Props(classOf[LockManagerActor]), name = "lockManager")

  import LockManager._

  def lock[T](name: String, expires: FiniteDuration)(body: => T): T = {
    var id: Long = 0
    try {
      val p1 = Promise[Long]()
      lockActor ! GetLock(name, expires, p1)
      id = Await.result(p1.future, 10 seconds)
      body
    } finally {
      val p2 = Promise[Unit]()
      lockActor ! FreeLock(name, id, p2)
      Await.result(p2.future, 10 seconds)
    }
  }

  def close() {
    // TODO shut down actor
    // TODO wait until closed
  }
}

object LockManager {

  case class LockInfo(id: Long, timer: Cancellable)

  case class GetLock(name: String, expires: FiniteDuration, p: Promise[Long])

  case class FreeLock(name: String, id: Long, p: Promise[Unit])

  case class TimeoutLock(name: String, id: Long)

  case class CloseLock()

}

class LockManagerActor() extends CheckedActor {

  import LockManager._

  private[this] implicit val ec: ExecutionContext = context.dispatcher

  private[this] var locks = Map[String, LockInfo]()
  private[this] var lockId: Long = 0

  def rec = {
    case GetLock(name, duration, p) =>
      if (locks.keySet.contains(name)) p.tryFailure(NotAvailableException(JsonObject("msg" -> "busy", "name" -> name)))
      lockId += 1
      val t = system.scheduler.scheduleOnce(duration) {
        self ! TimeoutLock(name, lockId)
      }
      val info = LockInfo(lockId, t)
      locks += (name -> info)
      p.trySuccess(lockId)
    case FreeLock(name, id, p) =>
      if (locks.keySet.contains(name)) {
        val info = locks(name)
        if (info.id == id) {
          info.timer.cancel()
          locks -= name
        }
        p.trySuccess(())
      }
    case TimeoutLock(name, id) =>
      if (locks.keySet.contains(name)) {
        val info = locks(name)
        if (info.id == id) {
          locks -= name
          log.error(noId, JsonObject("msg" -> "lock timeout", "name" -> name, "id" -> id))
        }
      }
  }
}
