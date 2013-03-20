package scala.react
import java.lang.ref.WeakReference

/**
 * Defines the base classes for the reactive framework.
 *
 * Note: The main backbone of the hierarchy is implemented in classes instead of traits for
 * efficiency reasons.
 */
abstract class ReactiveModule extends EngineModule { module: Domain =>
  object MuteControl extends scala.util.control.ControlThrowable

  /**
   * Control flow operator similar to `break`. May only be used inside a `mutable` block inside of
   * a `Reactive` implementation and indicates that the current reactive may not emit in
   * the current turn.
   */
  def mute: Nothing = throw MuteControl

  /**
   * Control flow operator similar to `breakable`. Retutrns `true` if `mute` was not called in `op`,
   * returns `false` otherwise. Some combinators use this internally, e.g., `Events.scanMutable`.
   */
  @inline final def mutable[B](op: => B): Boolean = try {
    op
    true
  } catch {
    case MuteControl => false
  }

  /**
   * Runs the given `op` on maximum level in this turn, i.e., `op` is safe to query any other
   * node.
   */
  def doLater(op: => Unit) {
    new DoLaterNode { def react() = op }
  }

  private abstract class DoLaterNode extends LeafNode {
    tick()
  }

  //type Dependents = PoolingWeakListSet[Node]
  //def newDependents(): Dependents = PoolingWeakListSet.empty

  abstract class Tickable {
    protected[react] def tick()
  }

  private object Node {
    final private val TickNumMask: Long = ~(TickInvalidMask | TickMuteMask)
    final private val TickInvalidMask: Long = 1L << 63
    final private val TickMuteMask: Long = 1L << 62
  }

  abstract class Node extends Tickable {
    /* lastTick indicates whether this node's pulse is valid (lastTick > 0) or invalid (< 0). Id 0
     * is reserved. This is used for multiple purposes.
     * When ticked, its sign bit is set, when validated the sign bit is cleared. Multiple ticks
     * are hence idempotent, and we can use this fact to make sure we don't put strict nodes on the
     * queue multiple times in the same turn. When emitting, lastTick is set to the current
     * turn id. Therefore, we can use this field to determine when this node has been emitting the
     * last time.
     */
    private var tickBits: Long = 1L

    private[react] val refs = new WeakReference(this)

    import Node._
    protected def isPulseValid = (tickBits & TickInvalidMask) == 0
    def lastTicked = tickBits & TickNumMask

    /**
     * Makes sure that the pulse of this node is consistent. Might be called multiple times in a
     * single turn. Calls to this method must happen from a level above this node. In other words,
     * if checkTopology() would throw an exception when this method is called, the behavior of
     * this method is undefined.
     */
    protected def validatePulse() {
      if (!isPulseValid) {
        doValidatePulse()
        setPulseValid()
      }
    }

    protected def doValidatePulse()

    protected def setPulseValid() {
      if (!isPulseValid) setNotEmitting()
    }

    /**
     * Mark as invalid and return 'true' if this node has not been invalidated in this turn yet,
     * otherwise do nothing but return `false`.
     */
    final protected[react] def invalidate(): Boolean = {
      val mask = engine.currentTurn | TickInvalidMask
      if (tickBits == mask) false
      else {
        tickBits = mask
        true
      }
    }

    def isEmitting: Boolean = tickBits == engine.currentTurn

    protected def setEmitting() {
      assert((engine.currentTurn & TickInvalidMask & TickMuteMask) == 0) // overflow
      tickBits = engine.currentTurn
    }
    protected def setNotEmitting() {
      tickBits = engine.currentTurn | TickMuteMask
    }

    engine.newNode(this)

    /**
     * The topological depth value of this node.
     *
     * Implementation note: this could be a def instead of a var for some nodes, which might reduce
     * object size. Benchmarking, however, revealed that this method then becomes a major bottleneck,
     * because of virtual calls and potential indirections because of traits and when accessing
     * the level of a dependency. Lesson learned: stick with a var for all nodes.
     */
    protected[react] var level = 0

    def name: String = debug.getName(this)
    def name_=(nme: String): this.type = { debug.setName(this, nme); this }

    def engine: Engine = module.engine

    /**
     * Called exactly once per turn for all nodes in the propagation set. Not called, if not in the
     * propagation set. As this method may be called out of topolical order, implementations should
     * not access this node's dependencies.
     */
    protected[react] def tick()

    /**
     * Ticks this nodes dependents. Does nothing, if or when this nodes does not have dependents.
     */
    protected def tickDependents()

    /**
     * Throws an exception if accessed from a level <= the level of this node. No-op otherwise.
     */
    def checkTopology() =
      if (level >= engine.currentLevel) engine.levelMismatch(this)

    /**
     * Makes sure that the value of this node is consistent. Might be called multiple times in a
     * single turn. Calls to this method must happen from a level above this node. In other words,
     * if checkTopology() would throw an exception when this method is called, the behavior of
     * this method is undefined.
     */
    protected def validateValue()

    def isDisposed: Boolean = tickBits == 0

    /**
     * Disconnects this node from the dependency graph.
     *
     * Subclasses that override this method should always call `super.disconnect()`
     * so effects of this method are chained.
     */
    protected def disconnect() { tickBits = 0 }

    def dispose() {
      disconnect()
    }

    def reactiveDescriptor: String = getClass.getName

    override def toString = {
      val nme = name
      val prefix = if (nme != null && nme != "") name + ":" else ""
      prefix + reactiveDescriptor + "@" + level
    }
  }

  /**
   * A node that does nothing.
   */
  trait MuteNode extends Node {
    protected[react] def tick() {}
    override def isEmitting = false
    override protected def tickDependents() {}
    override def checkTopology() {}
    override protected def validatePulse() {}
    override def doValidatePulse() {}
    override protected def validateValue() {}
  }

  object NilNode extends MuteNode

  /**
   * A dependent node that is strictly evaluated, i.e., in every turn in which it is ticked.
   *
   * When ticked, it lets the engine decide when it is safe to evaluate. It does not tick
   * its dependents until the engine tells this node to evaluate.
   */
  trait StrictNode extends Node {
    protected[react] def tick() {
      if (invalidate()) engine.defer(this)
    }

    /**
     * Called by the engine when this node defered itself in `tick`. Always called in topological
     * order, so implementations may safely access its dependencies.
     */
    protected[react] def tock() {
      validatePulse()
      if (isEmitting) tickDependents()
    }
  }

  /**
   * A dependent node that is evaluated not before queried, i.e., it might not be evaluated
   * in every turn in which it was ticked.
   *
   * A node that when ticked, always invalidates itself and ticks its dependents. It evaluates not
   * before it is queried.
   */
  trait LazyNode extends Node {
    invalidate()

    protected[react] def tick() {
      if (invalidate()) tickDependents()
    }
  }

  trait LeafNode extends StrictNode {
    level = Int.MaxValue
    protected def tickDependents() {}
    protected def isValueValid = true
    protected def doValidatePulse() { react(); setNotEmitting() }
    protected def validateValue() {}

    protected def react()
  }

  /**
   * A node that maintains a set of dependents, which are stored weakly. Methods that traverse
   * the dependent set clear it from stale weak refs.
   */
  abstract class DependencyNode extends Node {
    // Null if empty, a single weak ref or an array of weak refs if not empty.
    // This turns out to be the fastest and one of the most space efficient ways to handle weak
    // dependencies (few indirections, no garbage as long as array is not resized).
    private var dependents: AnyRef = null

    // Stuff in here is all inlined, since it is absolutely performance critical

    /**
     * Tick all dependents of this node.
     */
    protected def tickDependents() {
      dependents match {
        case null =>
        case wref: WeakReference[Node] =>
          val d = wref.get
          if ((d eq null) || !d.isInstanceOf[ManagesDependencies] /*|| d.isDisposed*/) dependents = null
          if (d ne null) d.tick()
        case arr: Array[WeakReference[Node]] =>
          var i = 0
          while (i < arr.length) {
            val wref = arr(i)
            if (wref ne null) {
              val d = wref.get
              if ((d eq null) || !d.isInstanceOf[ManagesDependencies] /*|| d.isDisposed*/) {
                arr(i) = null
              }
              if (d ne null) d.tick()
            }
            i += 1
          }
      }
    }

    /**
     * Transitively hoist all dependents to new level.
     */
    private[react] def hoistDependents(newLevel: Int) {
      // impl note: in contrast to tickDependents(), don't clear unmanaged dependencies
      dependents match {
        case null =>
        case wref: WeakReference[Node] =>
          val d = wref.get
          if (d eq null) dependents = null
          else engine.hoist(d, newLevel)
        case arr: Array[WeakReference[Node]] =>
          var i = 0
          while (i < arr.length) {
            val wref = arr(i)
            if (wref ne null) {
              val d = wref.get
              if (d eq null) arr(i) = null
              else engine.hoist(d, newLevel)
            }
            i += 1
          }
      }
    }

    /**
     * Add the given node to this node's dependents, if not already present.
     */
    protected[react] def subscribe(dep: Node) {
      if ((dep ne NilNode) && !isDisposed) {
        dependents match {
          case null => dependents = dep.refs
          case wref: WeakReference[Node] =>
            if (wref eq dep.refs) return
            val d = wref.get
            if (d eq null) dependents = dep.refs
            else if (d eq dep) return
            else {
              val newDeps = new Array[WeakReference[Node]](4)
              newDeps(0) = wref
              newDeps(1) = dep.refs.asInstanceOf[WeakReference[Node]]
              dependents = newDeps
            }
          case arr: Array[WeakReference[Node]] =>
            val oldSize = arr.length
            var i = 0
            while (i < oldSize) {
              val wref = arr(i)
              if (wref eq dep.refs) return
              if (wref eq null) {
                arr(i) = dep.refs.asInstanceOf[WeakReference[Node]]
                return
              } /*else {
                val d = wref.get
                if(d == null || d.isDisposed) {
                  arr(i) = dep.refs.asInstanceOf[WeakReference[Node]]
                  return
                }
              }*/
              i += 1
            }



            val newDeps = new Array[WeakReference[Node]](oldSize * 2)
            compat.Platform.arraycopy(arr, 0, newDeps, 0, oldSize)
            newDeps(oldSize) = dep.refs.asInstanceOf[WeakReference[Node]]
            dependents = newDeps
        }
      }
    }

    /**
     * Removes the given node from this node's dependent set if present.
     */
    protected[react] def unsubscribe(dep: Node) {
      if (dep ne NilNode) {
        dependents match {
          case null =>
          case wref: WeakReference[Node] =>
            val d = wref.get
            if ((d eq null) || (d eq dep)) dependents = null
          case arr: Array[WeakReference[Node]] =>
            var i = 0
            while (i < arr.length) {
              val wref = arr(i)
              if (wref ne null) {
                val d = wref.get
                if (d eq null) arr(i) = null
                else if (d eq dep) {
                  arr(i) = null
                  return
                }
              }
              i += 1
            }
        }
      }
    }

    protected override def disconnect() {
      dependents = null
      super.disconnect()
    }
  }

  /**
   * A marker trait indicating that this node takes care of subscribing and unsubscribing itself to its
   * dependencies.
   *
   * Implementation note: do not turn this trait into a boolean method. An instanceof check
   * is much faster than a virtual trait call.
   */
  trait ManagesDependencies extends Node

  /**
   * Convenience implementation for a dependent with a single input.
   */
  trait Dependent1[+P, +V] extends Node with ManagesDependencies {
    protected def input: Reactive[P, V]
    //level = input.level + 1

    protected def connect() {
      input.subscribe(this)
    }

    protected override def disconnect() {
      input.unsubscribe(this)
      super.disconnect()
    }
  }

  trait Dependent2[+P, +V] extends Node with ManagesDependencies {
    protected def input1: Reactive[P, V]
    protected def input2: Reactive[P, V]
    //level = math.max(input1.level, input2.level) + 1

    protected def connect() {
      input1.subscribe(this)
      input2.subscribe(this)
    }

    protected override def disconnect() {
      input1.unsubscribe(this)
      input2.unsubscribe(this)
      super.disconnect()
    }
  }

  trait DependentN extends Node with ManagesDependencies {
    protected def inputs: Iterable[Reactive[Any, Any]]
    // level = inputs.foldLeft(0) { (l, r) => math.max(l, r.level) } + 1

    protected def connect() {
      inputs foreach { _ subscribe this }
    }

    protected override def disconnect() {
      inputs foreach { _ unsubscribe this }
      super.disconnect()
    }
  }

  /**
   * A node that emits pulses and holds values. It has three properties that can change from turn
   * to turn and are updated either strictly or lazily:
   *
   * - whether this reactive is emitting
   * - if it is emitting, what pulse it is emitting
   * - what value it currently holds.
   *
   * A reactive must change its value only in a turn in which it is also emitting. It can,
   * however, choose not to change its value when it is emitting.
   */
  abstract class Reactive[+P, +V] extends DependencyNode with Emitter[P, V] {
    /**
     * Returns the value of this reactive if defined. The result is undefined, if this reactive
     * isn't currently emitting.
     */
    def getValue: V

    def valueNow: Option[V] = ifDefinedElse[Option[V]](Some(_))(None)

    def isDefined: Boolean

    protected def validateValue() {
      if (!isValueValid) doValidateValue()
    }

    protected def doValidateValue()
    protected def isValueValid: Boolean

    @inline final def ifDefined[U](f: V => U) {
      checkTopology()
      validateValue()
      if (isDefined) f(getValue)
    }

    @inline final def ifDefinedElse[B](f: V => B)(default: => B): B = {
      checkTopology()
      validateValue()
      if (isDefined) f(getValue) else default
    }

    /**
     * Returns a pulse value which is only valid if this reactive is currently emitting.
     * The result is undefined (it may throw an exception), if this reactive isn't currently
     * emitting.
     */
    def getPulse: P

    def pulseNow: Option[P] = ifEmittingElse[Option[P]](Some(_))(None)

    /**
     * Runs the given function with the current value of this pulse, if this pulse is currently
     * present.
     */
    @inline final def ifEmitting[U](f: P => U) {
      checkTopology()
      validatePulse()
      assert(isPulseValid)
      if (isEmitting) f(getPulse)
    }

    /**
     * Runs the given function with the current value of this pulse and return the result,
     * if this pulse is currently present. Otheriwse, returns the given default value.
     */
    @inline final def ifEmittingElse[B](f: P => B)(default: => B): B = {
      checkTopology()
      validatePulse()
      assert(isPulseValid)
      if (isEmitting) f(getPulse) else default
    }

    def changes: Events[P] = new ChangeEvents[P](this)

    override def toString = {
      val s = if (isPulseValid) "<valid>" else "<invalid>"
      val p = if (isEmitting) "p=" + getPulse else "<mute>"
      val v = if (isDefined) "v=" + getValue else "<undef>"
      super.toString + "(" + s + "," + p + "," + v + ")"
    }
  }

  /**
   * A reactive that always has a value, i.e., for which `isDefined == true` always holds.
   */
  abstract class TotalReactive[+P, +V] extends Reactive[P, V] {
    def now: V = {
      checkTopology()
      validateValue()
      getValue
    }

    def apply(): V = {
      checkTopology()
      subscribe(depStackTop)
      validateValue()
      getValue
    }

    def isDefined: Boolean = true
  }

  /**
   * A standard interface for an implementation of a reactive that can emit pulses. Any reactive
   * that emits pulses must extend this trait or one of its subtraits.
   */
  trait Emitter[+P, +V] extends Node { this: Reactive[P, V] =>
    /**
     * Sets this reactive's state to 'not emitting'.
     */
    protected[this] def mute() {
      assert(!isEmitting)
      freePulse()
      setNotEmitting()
    }

    /**
     * Lets this reactive emit the given pulse, i.e., sets the pulse and the state to 'emitting'.
     *
     * Unfortunately, this needs to stay untyped until SI-3272 is fixed, if that'll ever happen.
     */
    protected[this] def emit(p: Any)

    /**
     * Frees garbage from pulse
     */
    protected[react] def freePulse()
  }

  /**
   * An emitter whose pulses represent the current value as opposed to a change delta. Therefore,
   * the current value depends on the most recent pulse only and not on a previous value or pulse.
   */
  trait SimpleEmitter[+P, +V] extends Emitter[P, V] { this: Reactive[P, V] =>
    // TODO: I'd like to move most of that stuff here to Emitter, but that triggers SI-3272 or
    // some variation.
    protected[this] var pulse: P = _

    def getPulse = pulse

    protected[this] def emit(p: Any) {
      //val oldPulse = getPulse
      //val oldValue = getValue
      pulse = p.asInstanceOf[P]
      val doEmit = true //!isDefined //|| shouldEmit(oldValue, getPulse)
      if (doEmit) setEmitting()
      //else {
      //  pulse = oldPulse
      //  mute()
      // }
    }

    protected[react] def freePulse() { pulse = null.asInstanceOf[P] }
  }

  /**
   * An emitter whose pulses represent a delta to its previous value. Therefore, the current value
   * depends on the most recent pulse and a previous value.
   */
  trait DeltaEmitter[+P, +V] extends Emitter[P, V] { this: Reactive[P, V] =>

  }

  trait SimpleReactive[+P, +V] extends Reactive[P, V] with SimpleEmitter[P, V] {
    protected def isValueValid = isPulseValid

    protected def doValidateValue() { doValidatePulse() }
  }

  trait DeltaReactive[+P, +V] extends Reactive[P, V] with DeltaEmitter[P, V]

  /**
   * A reactive that accesses dependencies in a manner unpredictable by the framework. Usually
   * this means it evaluates some closure when notified, such as a `FuncSignal`.
   */
  trait OpaqueReactive[P, V] extends Reactive[P, V] {
    level = math.max(1, level)

    protected def doValidatePulse() {
      depStackPush(this)
      try {
        // eval and let reactives consulted in `op` obtain our dependent
        react()
      } finally {
        // always leave the dependent stack in a clean state.
        depStackPop()
      }
    }

    protected def react()
  }

  /**
   * Objects that create observers must inherit this trait. Maintains an internal list of created
   * observers. Once an instance of this trait is disposed, all it observers it maintains are
   * disposed.
   *
   * It is safe to create instances of this trait off-turn. Observers must be created in a turn,
   * however.
   */
  trait Observing { outer =>
    private var _obRefs: AnyRef = null

    protected abstract class Observer extends LeafNode {
      ref()

      protected override def disconnect() {
        super.disconnect()
        unref()
      }

      protected def ref() {
        _obRefs = _obRefs match {
          case null => this
          case ob: Observer => collection.immutable.Set(ob, this)
          case obs: Set[_] => obs.asInstanceOf[Set[Observer]] + this
          case _ => sys.error("Impossible case")
        }
      }

      protected def unref() {
        _obRefs = _obRefs match {
          case null => null
          case x if x eq this => null
          case obs: Set[_] => obs.asInstanceOf[Set[Observer]] - this
          case _ => sys.error("Impossible case")
        }
      }

      override def reactiveDescriptor = "Observer@" + System.identityHashCode(this)
    }

    class Observer1[P, V](val input: Reactive[P, V], op: P => Unit) extends Observer with Dependent1[P, V] {
      input.subscribe(this)
      def react() {
        if (!isDisposed) input.ifEmitting(op)
      }
    }

    class ObserverOnce1[P, V](input: Reactive[P, V], op: P => Unit) extends Observer1[P, V](input, op) {
      override def react() {
        super.react()
        disconnect()
      }
    }

    class ObserverDynamic(op: => Unit) extends Observer {
      def react() {
        depStackPush(this)
        try {
          op
        } finally {
          depStackPop()
        }
      }
    }

    /**
     * Observes pulses from the given reactive, starting in the current turn.
     */
    protected def observe[P, V](r: Reactive[P, V])(op: P => Unit): Observer = {
      val ob = new Observer1(r, op)
      ob.tick()
      ob
    }

    /**
     * Observes exactly one pulse from the given reactive, starting in the current turn.
     */
    protected def observeOnce[P, V](r: Reactive[P, V])(op: P => Unit): Observer = {
      new ObserverOnce1(r, op)
    }

    /**
     * Runs `op` once, which might access signals through `Signal.apply`. Repeatedly runs `op`
     * when some of the signals accessed change. Might run `op` even if no signal has changed.
     */
    protected def observeDynamic(op: => Unit): Observer = {
      val ob = new ObserverDynamic(op)
      ob.tick()
      ob
    }
  }

  trait Source[P, V] extends Reactive[P, V] {
    protected[this] def owner: Owner
    level = owner.level
    protected[react] def channel: Channel[P]
  }

  trait SimpleSource[P, V] extends SimpleReactive[P, V] with Source[P, V] {
    protected def doValidatePulse() {
      val x = channel.get(this)
      emit(x)
    }
  }

  trait Channel[P] {
    def get(r: Reactive[P, Any]): P
    def push(r: Reactive[P, Any], p: P)
  }

  sealed trait Owner {
    protected[react] def level: Int
    def newChannel[P, V](r: Reactive[P, V], p: P): Channel[P]
  }

  object DomainOwner extends Owner {
    def level = 0
    def newChannel[P, V](r: Reactive[P, V], p: P): Channel[P] = new LocalDelayChannel(r, p)
  }

  abstract class Router(inputNodes: Reactive[Any, Any]*) extends DependentN with StrictNode with Owner {
    protected val inputs = inputNodes.toSeq
    connect()
    protected implicit def owner: Owner = this

    invalidate()

    def newChannel[P, V](r: Reactive[P, V], p: P): Channel[P] = null

    protected def tickDependents() {}

    protected def doValidatePulse() {
      react()
    }

    protected def validateValue() {}

    //def get[P](r: Reactive[P, Any]): P = r.getPulse
    //def push[P](r: Reactive[P, Any], p: P) = r emit p

    def react()
  }

  implicit def owner: Owner = DomainOwner
}