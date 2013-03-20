package scala.react
package test.utils

import java.util.ArrayDeque
import org.junit.Assert._

object dom extends TestDomain

class TestDomain extends Domain {
  var engine = new TestEngine
  val scheduler = new ManualScheduler

  private val postTurnTodos = new ArrayDeque[() => Unit]
  def schedulePostTurn(op: => Unit) = postTurnTodos add (() => op)

  private def reset() {
    turnQueue.clear()
    postTurnTodos.clear()
    engine = new TestEngine
  }

  // add some test hooks to the standard engine
  class TestEngine extends Engine {
    override def runTurn() = super.runTurn()
    override def propagate() = {
      super.propagate()
      level = Int.MaxValue
      while (!postTurnTodos.isEmpty)
        postTurnTodos.poll().apply()
    }
    override def uncaughtException(e: Throwable) = {
      e.printStackTrace()
      postTurnTodos.clear()
      throw e
    }
  }

  def testTurns(turnsExpected: Int)(op: => Unit) {
    reset()
    dom.start()
    turn { op }
    while (!turnQueue.isEmpty) {
      val t = turnQueue.poll()
      dom schedule { t() }
      dom.engine.runTurn()
    }
    assertEquals(turnsExpected, engine.currentTurn - 1)
  }

  private val turnQueue = new ArrayDeque[() => Unit]

  // First run the given op, and then a turn.
  def turn(op: => Unit) {
    turnQueue add (() => op)
  }

  // run at the very end of current turn
  def postTurn(op: => Unit) = dom.schedulePostTurn(op)

  override def toString = "TestDomain"
}