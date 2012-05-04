package scala.react
package test

import utils._
import dom._
import org.junit.Test
import org.junit.Assert._

class RouterTests extends TestUtils with Observing {
  @Test def holdRouter() = testTurns(6) {
    val x = EventSource[Int]
    val r = new Router(x) {
      private val v = Var(1)
      def out: Signal[Int] = v
      def react() {
        x.ifEmitting { e =>
          v() = e
        }
      }
    }
    observe(r.out)(log(_))

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

  /*@Test def bidirectionalRouter() = testTurns(6) {
    val x = Var(1)
    val y = Var(2)
    val r = new Router(x) {
      private val v1 = Var(1)
      private val v2 = Var(2)
      def out1: Signal[Int] = v1
      def out2: Signal[Int] = v2

      def react() {
        x.ifEmitting { e =>
          v1() = e * v2.now
        }
        y.ifEmitting { e =>
          v2() = e * v1.now
        }
      }
    }
    observe(r.out1)(log(_))
    observe(r.out2)(log(_))

    turn {
      log.assert()
      x()= 1
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
    turn {
      log.assert()
      x()= 3
      log.assert()
    }
    turn {
      log.assert(3)
    }
  }*/
}