package scala.react

import scala.util.continuations._

trait SignalModule { module: Domain =>
  def Strict[A](op: =>A): Signal[A] = new StrictOpSignal(op)
  def Lazy[A](op: =>A): Signal[A] = new LazyOpSignal(op)

  object Signal {
    /**
     * Creates a flow signal that runs through the given `op` once.
     */
    def flow[A](init: A)(op: SignalFlowOps[A]=>Unit @suspendable): Signal[A] = new FlowSignal(init) {
      def body = op(this)
    }

    /**
     * Creates a flow signal that runs through the given `op` repeatedly until disposed, e.g., by
     * calling `halt`.
     */
    def loop[A](init: A)(op: SignalFlowOps[A]=>Unit @suspendable): Signal[A] = new FlowSignal(init) {
      def body = while(!isDisposed) op(this)
    }
  }

  /**
   * A time-varying value.
   */
  abstract class Signal[+A] extends TotalReactive[A, A] with SimpleReactive[A, A] { outer =>
    /* A signal's pulse and value are really the same, except the flag that indicates whether
     * the signal is emitting or not.
     */
    def getValue: A = getPulse

    def toStrict: Signal[A]

    // Fix types, when/if SI-3272 gets fixed
    protected def shouldEmit(oldVal: Any, newVal: Any): Boolean = oldVal != newVal

    override protected[this] def emit(p: Any) {
      if(shouldEmit(getValue, p)) super.emit(p)
    }

    override protected[react] def freePulse() {
      // the pulse is the value for signals, so don't do anything here.
    }
  }

  abstract class FuncSignal[A] extends Signal[A] with OpaqueReactive[A, A] {
    protected def react() {
      val v = eval
      emit(v)
    }

    protected def eval: A
  }

  abstract class StrictFuncSignal[A] extends FuncSignal[A] with StrictNode {
    tick()

    def toStrict: Signal[A] = this
  }

  class StrictOpSignal[A](op: => A) extends StrictFuncSignal[A] {
    protected def eval = op

    override def reactiveDescriptor = "Strict Signal"
  }

  abstract class LazyFuncSignal[A] extends FuncSignal[A] with LazyNode

  class LazyOpSignal[A](op: => A) extends LazyFuncSignal[A] {
    protected def eval = op

    def toStrict: Signal[A] = new StrictOpSignal(op)
  }

  /**
   * A signal with a single input.
   */
  abstract class Signal1[+A, +B](protected val input: Reactive[A, Any])
    extends Signal[B]
    with Dependent1[A, Any] {

    input.subscribe(this)

    def doValidatePulse() {
      input.ifEmitting(pulsate _)
      setPulseValid()
    }
    protected[this] def pulsate(a: A)
  }

  abstract class StrictSignal1[+A, +B](input: Reactive[A, Any])
    extends Signal1[A, B](input)
    with StrictNode

  protected[this] class HoldSignal[P](init: P)(input: Reactive[P, Any]) extends StrictSignal1[P, P](input) {
    pulse = init

    def pulsate(p: P) { emit(p) }

    def toStrict = this
  }

  object Var {
    /**
     * Creates a mutable signal with the given owner.
     */
    def apply[A](init: A)(implicit owner: Owner): Var[A] = new Var(init, owner)
  }

  /**
   * An externally mutable signal.
   */
  class Var[A] protected(init: A, val owner: Owner) extends Signal[A] with SimpleSource[A, A] with StrictNode {
    pulse = init

    protected[react] val channel = owner.newChannel(this, init)

    def toStrict = this

    /**
     * Sets the value of this signal, either in the next turn, if the owner is the domain or
     * in the current turn if the owner is a router.
     */
    def update(a: A) {
      if(owner eq DomainOwner) channel.push(this, a)
      else {
        emit(a)
        if(isEmitting) engine.defer(this)
      }
    }

    override def reactiveDescriptor = "Var"
  }

  /**
   * A constant signal.
   */
  case class Val[A](a: A) extends Signal[A] with MuteNode {
    pulse = a

    def toStrict = this
    override def reactiveDescriptor = "Val"
  }
}