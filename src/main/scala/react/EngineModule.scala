package scala.react

import java.util.ArrayDeque
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable.ArrayStack

abstract class EngineModule { self: Domain =>
  // Dependent stack stuff is inlined, since it needs to be as efficient as possible
  private var depStack = new Array[Node](4)
  private var depStackSize = 0

  protected def depStackPush(n: Node) {
    if (depStackSize == depStack.length) {
      val newarr = new Array[Node](depStack.length * 2)
      System.arraycopy(depStack, 0, newarr, 0, depStack.length)
      depStack = newarr
    }
    depStack(depStackSize) = n
    depStackSize += 1
  }

  protected def depStackPop() {
    if (depStackSize == 0) sys.error("Stack empty")
    depStackSize -= 1
    depStack(depStackSize) = null
  }

  protected def depStackTop: Node = depStack(depStackSize-1)


  // -----------------------------------------------------------------


  /**
   * Nodes that throw this exception must ensure that the dependent stack is in clean state, i.e.,
   * no leftovers on the stack. Otherwise dynamic dependency tracking goes havoc.
   */
  private[react] object LevelMismatch extends util.control.NoStackTrace {
    var accessed: Node = _
  }

  @inline def runTurn(op: => Unit) {
    op
    engine.runTurn
  }

  @inline def runReadTurn(leaf: LeafNode) {
    engine.runReadTurn(leaf)
  }

  abstract class Propagator {
    // the current turn index, first usable turn has index 2. Values 0 and 1 are reserved.
    private var turn = 1

    // the current propagation level at which this engine is or previously was
    protected var level = 0

    private var propQueue: PropQueue[StrictNode] = System.getProperty("scala.react.propqueue", "prio").toLowerCase match {
      case "prio" => new PriorityQueue[StrictNode] {
        def priority(n: StrictNode) = n.level
      }
      case "topo" => new TopoQueue[StrictNode] {
        def depth(n: StrictNode) = n.level
      }
    }

    def currentTurn: Long = turn
    def currentLevel: Int = level

    protected def applyTodos()

    def runTurn() {
      turn += 1
      debug.enterTurn(currentTurn)

      try {
        propagate()
      } catch {
        case e => uncaughtException(e)
      } finally {
        propQueue.clear
        level = 0
        debug.leaveTurn(currentTurn)
      }
    }

    def runReadTurn(leaf: LeafNode) {
      turn += 1
      debug.enterTurn(currentTurn)

      try {
        level = leaf.level
        leaf.invalidate()
        tryTock(leaf)
      } catch {
        case e => uncaughtException(e)
      } finally {
        propQueue.clear
        level = 0
        debug.leaveTurn(currentTurn)
      }
    }

    protected def propagate() {
      val queue = propQueue
      applyTodos()
      while (!queue.isEmpty) {
        val node = queue.dequeue
        assert(node.level >= level)
        if (node.level > level)
          level = node.level
        tryTock(node)
        assert(depStackSize == 0)
      }
    }

    def newNode(n: Node) {}

    def defer(dep: StrictNode) {
      debug.assertInTurn()
      val depLevel = dep.level
      if (depLevel == level) // fast path
        tryTock(dep)
      else if (depLevel < level) // can happen for signals that are created on the fly
        hoist(dep, level + 1)
      else propQueue += dep // default path
    }

    /**
     * Exceptions are caught in the main propagation loop and passed to this method.
     * Override to customize exception handling. The default behavior is to print a stack
     * trace and `sys.exit`.
     */
    protected def uncaughtException(e: Throwable) {
      Console.err.println("Uncaught exception in turn!")
      e.printStackTrace()
      sys.exit(-1)
    }

    /**
     * Try to tock a node. Hoists the node, if on wrong level.
     */
    protected def tryTock(dep: StrictNode) = try {
      debug.logTock(dep)
      dep.tock()
    } catch {
      case lm @ LevelMismatch =>
        val lvl = lm.accessed.level
        debug.logLevelMismatch(dep, lm.accessed)
        hoist(dep, lvl + 1)
    }

    def levelMismatch(accessed: Node) {
      LevelMismatch.accessed = accessed
      throw LevelMismatch
    }

    /**
     * Change the level of the given node and reschedule for later in this turn.
     */
    def hoist(dep: StrictNode, newLevel: Int) {
      dep.level = newLevel
      dep match {
        case dep: DependencyNode => dep.hoistDependents(newLevel + 1)
        case _ =>
      }
      propQueue reinsert dep
    }

    def hoist(dep: Node, newLevel: Int) {
      dep.level = newLevel
      dep match {
        case dep: DependencyNode => dep.hoistDependents(newLevel + 1)
        case _ =>
      }
      dep match {
        case dep: StrictNode => propQueue reinsert dep
        case _ =>
      }
    }
  }

  class Engine extends Propagator {
    // todos that can be scheduled externally, thread-safe
    private val asyncTodos = new ConcurrentLinkedQueue[AnyRef] // Runnable or ()=>Unit
    // todos that can be scheduled only inside a turn, no need for thead-safety
    private val localTodos = new ArrayDeque[Tickable]

    def schedule(r: Runnable) = {
      asyncTodos.add(r)
      scheduler.ensureTurnIsScheduled()
    }
    def schedule(op: => Unit) = {
      asyncTodos.add(() => op)
      scheduler.ensureTurnIsScheduled()
    }

    def tickNextTurn(t: Tickable) = {
      //assert(!localTodos.contains(t))
      // TODO: make localTodos a set?
      if (!localTodos.contains(t)) {
        localTodos add t
        scheduler.ensureTurnIsScheduled()
      }
    }
    protected def applyTodos() {
      var t = asyncTodos.poll()
      while (t ne null) {
        debug.logTodo(t)
        t match {
          case r: Runnable => r.run()
          case f: Function0[_] => f()
        }

        t = asyncTodos.poll()
      }

      while (!localTodos.isEmpty) {
        val t = localTodos.poll()
        debug.logTodo(t)
        t.tick()
      }
    }

    //def newLocalChannel[P](r: Reactive[P, Any], p: P): Channel[P] = new LocalDelayChannel(r, p)
  }

  class LocalDelayChannel[P](val node: Reactive[P, Any], p: P) extends Tickable with Channel[P] {
    private var pulse: P = p
    private var added = false
    def push(r: Reactive[P, Any], p: P) {
      pulse = p
      if (!added) {
        added = true
        engine.tickNextTurn(this)
      }
    }
    def get(r: Reactive[P, Any]) = pulse

    def tick() {
      added = false
      node.tick()
    }
  }
}