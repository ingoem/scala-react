package scala.react

import java.util.WeakHashMap
import java.text.SimpleDateFormat
import scala.annotation.elidable
import scala.collection.mutable.ArrayBuffer

abstract class Debug[D <: Domain](val domain: D) {
  import domain._

  def setName(node: Node, name: String)
  def getName(node: Node): String


  // The thread on which this engine is currently running, `null` if not in a turn.
  @volatile private var thread: Thread = null

  /**
   * Returns `true` if the current thread is the thread the engine is running on.
   */
  @elidable(800) protected def isInTurn = Thread.currentThread == thread
  @elidable(800) protected def isInTurn_=(b: Boolean) {
    thread = if (b) Thread.currentThread else null
  }

  @elidable(800) def enterTurn(id: Long) {
    assert(!isInTurn, "Tried to run a turn before the previous one was finished.")
    logEnterTurn(id)
    isInTurn = true
  }
  @elidable(800) def leaveTurn(id: Long) {
    assert(isInTurn)
    logLeaveTurn(id)
    isInTurn = false
  }

  @elidable(800) def assertInTurn() = assert(isInTurn, "This method must be run on its domain " + this)

  @elidable(800) def logStart()
  @elidable(800) def logEnterTurn(id: Long)
  @elidable(800) def logLeaveTurn(id: Long)
  @elidable(800) def logTock(n: Node)
  @elidable(800) def logLevelMismatch(accessor: Node, accessed: Node)
  @elidable(800) def logTodo(t: Any)
}

class NilDebug[D <: Domain](dom: D) extends Debug(dom) {
  import domain._

  def setName(node: Node, name: String) { }
  def getName(node: Node): String = ""

  def logStart() {}
  def logEnterTurn(id: Long) {}
  def logLeaveTurn(id: Long) {}
  def logTock(n: Node) {}
  def logLevelMismatch(accessor: Node, accessed: Node) {}
  def logTodo(t: Any) {}
}

abstract class AbstractDebug[D <: Domain](dom: D) extends Debug(dom) {
  import domain._
  private val names = new WeakHashMap[Node, String]
  def setName(node: Node, name: String) { names.put(node, name) }
  def getName(node: Node): String = names.get(node)
}

abstract class PrintDebug[D <: Domain](dom: D) extends AbstractDebug(dom) {
  import domain._

  private val dateFormat = new SimpleDateFormat("d MMM yyyy HH:mm:ss.SSS")

  def log(s: String)

  private def time() = System.currentTimeMillis()
  private def timePrefix = "+" + (time()-startTime) + "ms: "
  private var startTime = 0L

  def logStart() {
    startTime = time()
    log("Start domain " + domain + " at " + dateFormat.format(startTime))
  }
  def logEnterTurn(id: Long) = log(timePrefix + "enter turn " + id)
  def logLeaveTurn(id: Long) = log(timePrefix + "leave turn " + id)
  def logTock(n: Node) = log("Tock " + n)
  def logLevelMismatch(accessor: Node, accessed: Node) = log("Level mismatch when " + accessor + " accessed " + accessed)
  def logTodo(t: Any) = log("Todo " + t)
}


class ConsoleDebug[D <: Domain](dom: D) extends PrintDebug(dom) {
  def log(s: String) = println(s)
}