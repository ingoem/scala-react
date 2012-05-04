package scala.react
package test

import utils._
import dom._
import org.junit.Test
import org.junit.Assert._

class CombinatorTests extends TestUtils with Observing {
  @Test def eventsMap() = testTurns(6) {
    val x = EventSource[Int]
    val y = x map (_ * 2)
    observe(y)(log(_))

    turn {
      log.assert()
      x << 1
      log.assert()
    }
    turn {
      log.assert(2)
      x << 1
      log.assert()
    }
    turn {
      log.assert(2)
    }
    turn {
      log.assert()
      x << 3
      log.assert()
    }
    turn {
      log.assert(6)
    }
  }


  @Test def eventsMapLazy() = testTurns(5) {
    val x = EventSource[Int]
    val y = x map (_ * 2)

    turn {
      x << 1
      doLater {
        assertEquals(Some(2), y.pulseNow)
      }
    }
    turn {
      x << 2
    }
    turn {
      x << 3
      observe(y) { log(_) }
    }
    turn {
      log.assert(6)
    }
  }

  @Test def eventsCollect() = testTurns(7) {
    val x = EventSource[Int]
    val y = x collect {
      case x if x % 2 == 0 => x
    }
    observe(y)(log(_))

    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert(2)
      x << 1
      log.assert()
    }
    turn {
      log.assert()
    }
    turn {
      log.assert()
      x << 3
      log.assert()
    }
    turn {
      log.assert()
      x << 22
      log.assert()
    }
    turn {
      log.assert(22)
    }
  }

  @Test def eventsFilter() = testTurns(7) {
    val x = EventSource[Int]
    val y = x filter (_ % 2 == 0)
    observe(y)(log(_))

    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert(2)
      x << 1
      log.assert()
    }
    turn {
      log.assert()
    }
    turn {
      log.assert()
      x << 3
      log.assert()
    }
    turn {
      log.assert()
      x << 22
      log.assert()
    }
    turn {
      log.assert(22)
    }
  }

  @Test def eventsTake() = testTurns(6) {
    val x = EventSource[Int]
    val y = x take 2
    observe(y)(log(_))

    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert(2)
      x << 11
      log.assert()
    }
    turn {
      log.assert(11)
      x << 3
      log.assert()
    }
    turn {
      log.assert()
      x << 4
      log.assert()
    }
    turn {
      log.assert()
    }
  }

  @Test def eventsDelay2() = testTurns(10) {
    val x = EventSource[Int]
    val y = x delay 2
    observe(y)(log(_))

    turn {
      log.assert()
      x << 1
      log.assert()
    }
    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert()
      x << 3
      log.assert()
    }
    turn {
      log.assert(1)
      x << 4
      log.assert()
    }
    turn {
      log.assert(2)
    }
    turn {
      log.assert()
    }
    turn {
      log.assert()
      x << 5
      log.assert()
    }
    turn {
      log.assert(3)
      x << 6
      log.assert()
    }
    turn {
      log.assert(4)
    }
  }

  @Test def eventsScan() = testTurns(6) {
    val x = EventSource[Int]
    val y = x.scan(1){ _ + _ }
    observe(y)(log(_))

    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert(3)
      x << 5
      log.assert()
    }
    turn {
      log.assert(8)
    }
    turn {
      log.assert()
      x << 5
      log.assert()
    }
    turn {
      log.assert(13)
    }
  }

  @Test def eventsScan1() = testTurns(6) {
    val x = EventSource[Int]
    val y = x.scan1 { _ + _ }
    observe(y)(log(_))

    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert(2)
      x << 5
      log.assert()
    }
    turn {
      log.assert(7)
    }
    turn {
      log.assert()
      x << 5
      log.assert()
    }
    turn {
      log.assert(12)
    }
  }

  @Test def eventsScanMutable() = testTurns(8) {
    val x = EventSource[Int]
    val y = x.scanMutable("0") { (acc, e) =>
      if(e == 2) mute else acc + "," + e
    }
    observe(y)(log(_))

    turn {
      log.assert()
      x << 1
      log.assert()
    }
    turn {
      log.assert("0,1")
      x << 3
      log.assert()
    }
    turn {
      log.assert("0,1,3")
    }
    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert()
      x << 4
      log.assert()
    }
    turn {
      log.assert("0,1,3,4")
    }
    turn {
      log.assert()
    }
  }

  @Test def signalChanges() = testTurns(6) {
    val x = Var(0)
    val y = x.changes
    observe(y)(log(_))

    turn {
      log.assert()
      x()= 1
      log.assert()
    }
    turn {
      log.assert(1)
      x()= 1
      log.assert()
    }
    turn {
      log.assert()
    }
    turn {
      log.assert()
      x()= 2
      log.assert()
    }
    turn {
      log.assert(2)
    }
  }

  @Test def eventsHold() = testTurns(6) {
    val x = EventSource[Int]
    val y = x.hold(1)
    observe(y)(log(_))

    turn {
      log.assert()
      x << 1
      log.assert()
    }
    turn {
      log.assert()
      x << 2
      log.assert()
    }
    turn {
      log.assert(2)
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

  @Test def strictLazyReorder() = testTurns(6) {
    val switch = Var(false)
    val a0 = Var(0) // level 0
    val a1 = Strict { a0() + 1 } // level 1
    //val s2 = Strict { if(s0() % 2 == 0) s0() + 1 else s1() + 2 } // level 1 then 2
    val a2 = Strict { a1() + 1 } // level 2
    val a3 = Strict { a2() + 1 } // level 3
    val a14 = Strict { if(switch()) a3() + 1 else a0() + 1  } // level 1 or 4
    val l25 = Lazy { a14() + 1 } // level 2 or 5
    // this should always observe the correct state in l25, which needs to be lifted
    // transitively when a14 switches branches
    val res = Strict {
      val x = a0() + l25() + 1
      log("eval" + x) // log to make sure this signal isn't evaluated redundantly
      x
    }
    observe(res)(log(_))

    turn {
      log.assert("eval3", 3)
      assertEquals(0, a0.level)
      assertEquals(1, a1.level)
      assertEquals(2, a2.level)
      assertEquals(3, a3.level)
      assertEquals(1, a14.level)
      assertEquals(1, l25.level) // not evaluated, should be something > 0
      assertEquals(2, res.level)

      a0()= 1
      log.assert()
    }
    turn {
      log.assert("eval5", 5)
      assertEquals(0, a0.level)
      assertEquals(1, a1.level)
      assertEquals(2, a2.level)
      assertEquals(3, a3.level)
      assertEquals(1, a14.level)
      assertEquals(1, l25.level)
      assertEquals(2, res.level)

      a0()= 2
      switch()= true

      log.assert()
    }
    turn {
      log.assert("eval10", 10)
      assertEquals(0, a0.level)
      assertEquals(1, a1.level)
      assertEquals(2, a2.level)
      assertEquals(3, a3.level)
      assertEquals(4, a14.level)
      assertEquals(5, l25.level)
      assertEquals(6, res.level)
    }
    turn {
      log.assert()
      a0()= 3
      log.assert()
    }
    turn {
      log.assert("eval12", 12)
    }
  }
}