package scala.react
package test

import utils._
import dom._
import org.junit.Test
import org.junit.Assert._

class ReactorTests extends TestUtils with Observing {
  @Test def simple() = testTurns(3) {
    val x = EventSource[Int]
    val r = Reactor.flow { self =>
      log(1)
      self await x
      log(2)
    }

    turn {
      log.assert(1)
      x << 1
      log.assert()
    }
    turn {
      log.assert(2)
      x << 2
      log.assert()
    }
  }
}