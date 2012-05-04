package scala.react
package test

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.ShouldMatchersForJUnit._
import test.utils._
import dom._

class FlowTests extends TestUtils {

  @Test def simpleSignal() = testTurns(2) {
    val x = Signal.flow(1) { self =>
      log("a")
    }
    logReactive(x)

    postTurn {
      log.assert("a")
      assertEquals(1, x.now)
    }

    turn {
      postTurn {
        log.assert()
      }
    }
  }

  @Test def simpleSeqSignal() = testTurns(4) {
    val x = Signal.flow(1) { self =>
      log("a")
      self()= 2
      self.pause
      log("b")
      self()= 3
      self.pause
      log("d")
    }
    logReactive(x)

    postTurn {
      log.assert("a", 2)
      assertEquals(2, x.now)
    }

    turn {
      postTurn {
        log.assert("b", 3)
        assertEquals(3, x.now)
      }
    }
    turn {
      postTurn {
        log.assert("d")
      }
    }
    turn {
      postTurn {
        log.assert()
      }
    }
  }

  @Test def simpleSeqSignalWithPause() = testTurns(5) {
    val x = Signal.flow(0) { self =>
      log("a")
      self()= 1
      self.pause
      log("b")
      self.pause
      log("c")
      self()= 2
      self.pause
      log("d")
    }
    logReactive(x)

    postTurn {
      log.assert("a", 1)
    }

    turn {
      postTurn {
        log.assert("b")
      }
    }
    turn {
      postTurn {
        log.assert("c", 2)
      }
    }
    turn {
      postTurn {
        log.assert("d")
      }
    }
    turn {
      postTurn {
        log.assert()
      }
    }
  }

  @Test def simpleNext() = testTurns(6) {
    val es1 = EventSource[Int]
    val es2 = EventSource[Int]
    val x = Signal.loop(1) { self =>
      log("a")
      self()= (self await es1)
      log("b")
      self.pause
      log("c")
      self() = (self await es1)
      log("d")
      self.pause
      log("e")
      self() = (self await es2)
      log("f")
      self.pause
    }
    logReactive(x)

    postTurn {
      log.assert("a")
      assertEquals(1, x.now)
    }

    turn {
      es1 << 2
      postTurn {
        log.assert("b", 2)
        assertEquals(2, x.now)
      }
    }
    turn {
    }
    turn {
      es1 << 3
      postTurn {
        log.assert("c", "d", 3)
        assertEquals(3, x.now)
      }
    }
    turn {
      es1 << 4
      postTurn {
        log.assert("e")
        assertEquals(3, x.now)
      }
    }
    turn {
      es2 << 5
      postTurn {
        log.assert("f", 5)
        assertEquals(5, x.now)
      }
    }
  }

  @Test def finishedFlowIsNotMute() = testTurns(2) {
    val x = Signal.flow(0) { self =>
      log("a")
      self()= 1
      log("b")
    }
    logReactive(x)

    postTurn {
      log.assert("a", "b", 1)
    }

    turn {
      postTurn {
        log.assert()
      }
    }
  }

  @Test def repeatedEmit() = testTurns(2) {
    val x = Signal.flow(0) { self =>
      log("a")
      self()= 1
      log("b")
      self()= 2
      log("c")
    }
    logReactive(x)

    postTurn {
      log.assert("a", "b", "c", 2)
    }

    turn {
      postTurn {
        log.assert()
      }
    }
  }

  @Test def parJoin() = testTurns(3) {
    val x = Signal.flow(1) { self =>
      log("1")
      self.par {
        log("a1")
        self.pause
        log("a2")
        self.join
        log("a3")
      } {
        log("b1")
        self.pause
        log("b2")
        self.pause
        log("b3")
      }
      log("2")
    }
    logReactive(x)

    postTurn {
      log.assert("1", "a1", "b1")
    }

    turn {
      postTurn {
        log.assert("a2", "2")
      }
    }
    turn {}
  }

  @Test def nestedParsJoin() = testTurns(3) {
    val x = Signal.flow(1) { self =>
      log("1")
      self.par {
        log("a1")
        self.par {
          log("c1")
          self.pause
          log("c2")
          self.join
          log("c3") // should never be reached
        } {
          log("d1")
          self.pause
          log("d2") // should never be reached
        }
        log("a2")
      } {
        log("b1")
        self.pause
        log("b2")
      }
      log("2")
    }
    logReactive(x)

    postTurn {
      log.assert("1", "a1", "c1", "d1", "b1")
    }

    turn {
      postTurn {
        log.assert("c2", "a2", "b2", "2")
      }
    }
    turn {}
  }

  @Test def loopUntilSignal() = testTurns(6) {
    val es1 = EventSource[Int]
    val es2 = EventSource[Int]
    val x = Signal.flow(1) { self =>
      log("a")
      var i = 2
      val e1 = self.loopUntil(es1) {
        log("b")
        self()= i
        self.pause
        i += 1
        log("c")
      }
      log("d")
      self()= e1
      self.pause
      log("e")
      val e2 = self.loopUntil(es2) {
        log("f")
        self()= i
        self.pause
        i += 1
        log("g")
      }
      log("h")
    }.name = "subject"
    logReactive(x)

    postTurn {
      log.assert("a", "b", 2)
    }

    turn {
      postTurn {
        log.assert("c", "b", 3)
      }
    }
    turn {
      es1 << 1
      postTurn {
        log.assert("d", 1)
      }
    }
    turn {
      postTurn {
        log.assert("e", "f", 3) // last i+=1 should have been skipped
      }
    }
    turn {
      es1 << 2
      postTurn {
        log.assert("g", "f", 4)
      }
    }
    turn {
      es2 << 2
      postTurn {
        log.assert("h")
      }
    }
  }

  @Test def loopEndUntilSignal() = testTurns(6) {
    val es1 = EventSource[Int]
    val es2 = EventSource[Int]
    val x = Signal.flow(1) { self =>
      log("a")
      var i = 2
      val e1 = self.loopEndUntil(es1) {
        log("b")
        self()= i
        self.pause
        i += 1
        log("c")
      }
      log("d")
      self()= e1
      self.pause
      log("e")
      val e2 = self.loopEndUntil(es2) {
        log("f")
        self()= i
        self.pause
        i += 1
        log("g")
      }
      log("h")
    }.name = "subject"
    logReactive(x)

    postTurn {
      log.assert("a", "b", 2)
    }

    turn {
      postTurn {
        log.assert("c", "b", 3)
      }
    }
    turn {
      es1 << 1
      postTurn {
        log.assert("c", "d", 1)
      }
    }
    turn {
      postTurn {
        log.assert("e", "f", 4) // last i+=1 should have been executed
      }
    }
    turn {
      es1 << 2
      postTurn {
        log.assert("g", "f", 5)
      }
    }
    turn {
      es2 << 2
      postTurn {
        log.assert("g", "h")
      }
    }
  }

}