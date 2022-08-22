package scala.react
package test.utils

import org.junit.{ After, Before, Rule }
import org.junit.Assert._
import java.util.ArrayDeque
import org.scalatest.junit.ShouldMatchersForJUnit._
import scala.collection.mutable._
import dom._
import org.junit.BeforeClass
import org.junit.rules.TestName

abstract class TestUtils extends JTestUtils with Observing {
  val log = new Log

  class Log {
    private val buf = new ArrayBuffer[Any] with SynchronizedBuffer[Any]
    def apply(a: Any) {
      buf += a
    }
    def assert(as: Any*) { buf.toArray should equal(as.toArray); clear() }
    def clear() = buf.clear
  }


  @Before def setup() {
    log.clear()
    println("======= Running " + testName.getMethodName)
  }

  @After def tearDown() {
    log.assert()
    println("======= done with " + testName.getMethodName)
  }

  def logReactive[P](r: Reactive[P, Any]): Observer = observe(r){ log(_) }.name = "(Logger of " + r + ")"
}