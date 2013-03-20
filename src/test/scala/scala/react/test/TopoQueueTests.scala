package scala.react
package test

import org.junit.Test
import org.junit.Assert._

class TopoQueueTests {
  case class Node(i: Int)

  class Q extends TopoQueue[Node] {
    def depth(n: Node) = n.i
  }

  @Test def simple() {
    val q = new Q
    assertTrue(q.isEmpty)
    q.clear()
    assertTrue(q.isEmpty)
    q += Node(0)
    assertFalse(q.isEmpty)
    q += Node(0)
    q += Node(1)
    assertFalse(q.isEmpty)
    q += Node(5)

    assertEquals(Node(0), q.dequeue)
    assertFalse(q.isEmpty)
    assertEquals(Node(0), q.dequeue)
    assertEquals(Node(1), q.dequeue)
    assertFalse(q.isEmpty)
    assertEquals(Node(5), q.dequeue)
    assertTrue(q.isEmpty)
    q.clear()
    assertTrue(q.isEmpty)
  }

  @Test def withMaxLevel() {
    val q = new Q
    assertTrue(q.isEmpty)
    q += Node(Int.MaxValue)
    assertFalse(q.isEmpty)
    assertEquals(Node(Int.MaxValue), q.dequeue)
    assertTrue(q.isEmpty)
    q.clear()
    assertTrue(q.isEmpty)
  }

}