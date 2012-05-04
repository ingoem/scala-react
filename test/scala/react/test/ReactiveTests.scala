package scala.react
package test

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.ShouldMatchersForJUnit._
import test.utils._
import dom._

class ReactiveTests extends TestUtils {

  @Test def testTurnCount() = testTurns(1) {
  }

  @Test def testTurnOrder() = testTurns(4) {
    log(1)

    turn {
      log.assert(1)
      log.assert()
      log(2)
    }
    turn {
      log.assert(2)
      log.assert()
      log(3)
    }
    turn {
      log.assert(3)
      log.assert()
    }
  }

  @Test(expected=classOf[AssertionError])
  def testTurnOrderFailCount() = testTurns(3) {
    log(1)

    turn {
      log.assert(1)
      log.assert()
      log(2)
    }
    turn {
      log.assert(2)
      log.assert()
      log(3)
    }
    turn {
      log.assert(3)
      log.assert()
    }
  }

  @Test(expected=classOf[AssertionError])
  def testTurnOrderFailLog() = testTurns(2) {
    turn {
      log.assert(1)
    }
  }

  @Test def testDoLater() = testTurns(5) {
    doLater { log(1) }
    log.assert()
    postTurn {
      log.assert(1)
    }

    turn {
      log.assert()
    }
    turn {
      log.assert()
      doLater{ log(2) }
      postTurn {
        log.assert(2)
      }
    }
    turn {
      log.assert()
      doLater{ log(10) }
      doLater{ log(11) }
      postTurn {
        log.assert(10, 11)
      }
    }
    turn {
      log.assert()
    }
  }

  @Test def simpleInvalidate() = testTurns(1) {
    new Observer {
      assertTrue(invalidate())
      assertFalse(invalidate())

      def react() {}
    }
  }

  @Test def simpleObserveVar() = testTurns(7) {
    val x = Var(1)
    logReactive(x)

    doLater { assertEquals(1, x.now) }

    turn {
      log.assert()
      doLater {
        x() = 2
      }
    }
    turn {
      log.assert()
      postTurn { log.assert(2) }
    }
    turn {
      log.assert()
    }
    turn {
      log.assert()
      x() = 3
      doLater { log.assert() }
    }
    turn {
      log.assert(3)
      x() = 4
      x() = 5
      doLater { log.assert() }
    }
    turn {
      log.assert(5)
    }
  }

  @Test def simpleObserveVarImmediate() = testTurns(2) {
    val x = Var(1)
    x() = 2
    observe(x)(log(_))
    log.assert()
    doLater { log.assert(2) }

    turn {
      log.assert()
    }
  }

  @Test def simpleObserveEventSource() = testTurns(6) {
    val x = EventSource[Int]
    observe(x)(log(_))

    turn {
      log.assert()
      x << 1
      log.assert()
    }
    turn {
      log.assert(1)
      x << 1
      log.assert()
    }
    turn {
      log.assert(1)
    }
    turn {
      log.assert()
      x << 3
      log.assert()
    }
    turn {
      log.assert(3)
    }
  }

  @Test def eventsOnce() = testTurns(4) {
    val x = Events.once(1)
    observe(x)(log(_))
    log.assert()

    turn {
      log.assert(1)
    }
    turn {
      log.assert()
    }
    turn {
      log.assert()
    }
  }

  @Test def eventsNever() = testTurns(3) {
    val x = Events.never[Int]
    observe(x)(log(_))
    log.assert()

    turn {
      log.assert()
    }
    turn {
      log.assert()
    }
  }

  @Test def simpleObserveOnce() = testTurns(9) {
    val x = Var(1)
    observeOnce(x)(log(_))

    turn {
      log.assert()
      x() = 2
      log.assert()
    }
    turn {
      log.assert(2)
      x() = 2
      log.assert()
    }
    turn {

    }
    turn {
      log.assert()
      x() = 3
    }
    turn {
      log.assert()
    }

    turn {
      log.assert()
      x() = 4
      observeOnce(x)(log(_))
      postTurn { log.assert(4) }
    }
    turn {
      log.assert()
      x() = 5
    }
    turn {
      log.assert()
    }
  }

  @Test def strictSignalInit() = testTurns(3) {
    val x = Var(1)
    val y = Strict {
      log("y")
      log(x.now)
      log(x.getPulse)
      log(x())
    }

    turn {
      log.assert("y", 1, 1, 1)
      x() = 2
      log.assert()
    }
    turn {
      log.assert("y", 2, 2, 2)
    }
  }

  @Test def strictSignalHigherOrder() = testTurns(3) {
    // Testing that:
    // - we don't have assertions in the engine that break when nodes are created on the fly and/or
    // below the current engine level
    // - the dependent stack stays in a reasonable state in the presence of HO nodes and level
    // mismatches
    val x = Var(1) // level 0
    val y = Strict { x() + 1 } // level 1
    val z: Signal[Signal[Int]] = Strict {
      y() // lift this sig to level 2
      Strict {
        x()+1 // let this one be below the outer signal => below current engine level on first creation
      }.name = "inner"
    }.name = "outer"

    turn {
      x() = 2
      println(x)
    }
    turn {
      x() = 3
    }
  }

  @Test def strictSignalAdaptsLevel() = testTurns(3) {
    val x = Var(1)
    val y = Strict { x() + 1 }
    val z = Strict {
      log("z")
      log(y.now)
      log(y.getPulse)
      log(y())
    }

    turn {
      log.assert("z", "z", 2, 2, 2) // redundant "z", because of hoisting
      x() = 2
      log.assert()
    }
    turn {
      log.assert("z", 3, 3, 3)
    }
  }

  @Test def strictSignalAdaptsLevel2() = testTurns(5) {
    val x = Var(1)
    val y = Strict { x() + 1 }
    val which = Var(true)
    val z = Strict {
      if(which()) { log("then"); x() }
      else { log("else"); y() }
    }
    logReactive(z)

    postTurn {
      log.assert("then", 1)
    }

    turn {
      log.assert()
      x() = 2
      log.assert()
      postTurn {
        log.assert("then", 2)
      }
    }
    turn {
      log.assert()
      which() = false
      log.assert()
      postTurn {
        log.assert("else", "else", 3)
      }
    }

    turn {
      log.assert()
      x() = 3
      log.assert()
      postTurn {
        log.assert("else", 4)
      }
    }

    turn {
      log.assert()
      which() = true
      log.assert()
      postTurn {
        log.assert("then", 3)
      }
    }
  }



  @Test def strictSignalRedundantEval() = testTurns(3) {
    val x = Var(1)
    val y = Strict { x() }
    val z = Strict { x() }
    val sum = Strict{ y() + z() }
    observe(sum){ log(_) }

    turn {
      log.clear()
      x() = 2
      log.assert()
    }
    turn {
      log.assert(4)
    }
  }

  @Test def lazySignal() = testTurns(10) {
    val x = Var(1)
    val y = Lazy {
      log("y")
      val v = x.now
      log(v) // should adapt level
      log(x.getPulse)
      log(x())
      v
    }
    var doEval = Var(false)
    val z = Strict {
      if (doEval()) log("z " + y()) else log("z mute")
    }
    log.assert()

    turn {
      log.assert("z mute")
      x() = 2
      postTurn { log.assert() }
    }
    turn {
      // lazy signal is not connected
      log.assert()
      doLater {
        log("ob " + y.now)
      }
    }
    turn {
      log.assert("y", 2, 2, 2, "ob 2")
      // this should not cause y to validate, but invalidate it
      x() = 3
    }
    turn {
      // y should not have been evaluated
      log.assert()
      // should cause z to eval y
      doEval() = true
    }
    turn {
      log.assert("y", 3, 3, 3, "z 3")
      x() = 4
      log.assert()
    }
    turn {
      log.assert("y", 4, 4, 4, "z 4")
      doEval() = false
    }
    turn {
      log.assert("z mute")
      x() = 5
      log.assert()
    }
    turn {
      log.clear() // z might have been notified, because of a stale dependency
      x() = 6
      log.assert()
    }
    turn {
      // stale dependency should have been cleared
      log.assert()
    }
  }

}