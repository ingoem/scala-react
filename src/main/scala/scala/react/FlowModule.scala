package scala.react

import scala.util.continuations._

trait FlowModule { module: Domain =>
  private val idContinuation = ()=>()

  trait FlowNode extends StrictNode {
    protected[react] var _continue = initialContinuation

    protected def continue() = _continue()
    protected def initialContinuation: () => Unit = () => reset {
      body()
      //TODO: dispose()
    }

    override def dispose() {
      _continue = idContinuation
      super.dispose()
    }

    def body(): Unit @suspendable

    protected[react] def shiftAndContinue[A](body: (A => Unit) => Unit) =
      shift { (k: A => Unit) =>
        // we need to save the continuation before we run it, so we always have the very latest
        // continuation when a level mismatch exception is thrown.
        _continue = () => body(k)
        continue()
      }

    /**
     * Continues with the given continuation in the next cycle.
     */
    protected[react] def continueLater(k: => Unit) = {
      _continue = () => k
      engine.tickNextTurn(this)
    }

    /**
     * Continues with the given continuation and returns the remaining continuation.
     */
    protected[react] def continueWith(k: => Unit): (()=>Unit) = {
      _continue = () => k
      continue()
      _continue
    }
  }


  trait FlowOps { this: FlowNode =>

    /**
     * Suspends the reactive and continues execution in the next turn.
     */
    def pause: Unit @suspendable = shift { (k: Unit => Unit) =>
      continueLater { k() }
    }

    /**
     * Waits for a pulse from the given reactive and returns it. Returns immediately, if the
     * reactive is currently emitting.
     */
    def await[B](input: Reactive[B, Any]): B @suspendable = shiftAndContinue[B] { k =>
      input.ifEmittingElse { p =>
        /*if (!reactive.isDisposed)*/ k(p)
      } {
        input subscribe this
      }
    }

    /**
     * Waits for the next pulse from the given reactive and returns it. Never returns immediately,
     * even if the reactive is currently emitting. Equivalent to `pause; await input`
     */
    def awaitNext[B](input: Reactive[B, Any]): B @suspendable = {
      pause
      await(input)
    }

    /**
     * Halts the reactive and clears internal data structures.
     * Once halted, a reactive cannot resume.
     */
    def halt: Unit @suspendable = shift { (k: Unit => Unit) => dispose() }

    /**
     * Repeatedly executes `body` until the given reactive `r` emits.
     * Immediately returns if `r` is currently emitting. Otherwise, it finishes the iteration
     * in which `r` emits, i.e., the execution of the body is always completed before
     * returning. Equivalent to
     *
     * {{{
     * var done = false
     * var e: A
     * par {
     *   e = next(es)
     *   done = true
     * } {
     *   while (!done) { body }
     * }
     * e
     * }}}
     */
    def loopEndUntil[A](r: Reactive[A, Any])(body: => Unit @suspendable): A @suspendable = {
      var done = false
      par {
        await(r)
        done = true
      } {
        while (!done) { body }
      }
      //assert(r.isEmitting)
      r.getPulse
    }

    /**
     * Same as `loopEndUntil` but aborts the execution of `body` in the turn the given reactive
     * emits. Equivalent to
     *
     * {{{
     * var e: A
     * abortOn(r) {
     *   while (true) { body }
     * }
     * e
     * }}}
     */
    def loopUntil[A](r: Reactive[A, Any])(body: => Unit @suspendable): A @suspendable = {
      abortOn(r) {
        while (true) { body }
      }
      //assert(r.isEmitting)
      r.getPulse
    }

    /**
     * Runs the given `body` until the given reactive emits. Does not run `body` if the reactive
     * is currently emitting.
     */
    def abortOn[A](input: Reactive[A, Any])(body: => Unit @suspendable): Unit @suspendable = par {
      await(input)
      join
    } {
      body
      join
    }

    //private var _join = () => ()
    private var _join: scala.runtime.IntRef = null

    /**
     * Halts the execution of the current branch and, if present, joins the innermost
     * enclosing `par` expression. Only halts the current branch if there is no enclosing `par`.
     */
    def join: Unit @suspendable = shiftAndContinue[Unit] { k =>
      if(_join == null) throw new IllegalStateException("Join called in wrong context.")
      _join.elem = 0
    }

    /**
     * First runs the `left` branch until it pauses, finishes or calls `join`. Then does
     * the same with the `right` branch. If both branches finish, or at least one branch called
     * `join`, this method immediately returns. If no branch called `join` and at least one branch
     * is not finished yet, this method pauses and next turn, will continue evaluating each branch
     * where it stopped previously. It proceeds so until both branches are finsihed or at least one
     * calls `join`.
     */
    def par(left: => Unit @suspendable)(right: => Unit @suspendable): Unit @suspendable =
      shiftAndContinue[Unit] { exitK =>
        val latch = new scala.runtime.IntRef(2)

        def cont(left: => Unit)(right: => Unit): Unit = {
          val oldLatch = _join // stack joins for nested pars
          _join = latch
          val leftK = continueWith(left)
          if (latch.elem > 0) {
            val rightK = continueWith(right)
            _join = oldLatch
            if (latch.elem > 0) {
              _continue = () => cont { leftK() } { rightK() }
            } else exitK()
          } else exitK()
        }

        cont { reset { left; latch.elem -= 1 } } { reset { right; latch.elem -= 1 } }
      }
  }

  trait SimpleFlowOps[P, V, R <: SimpleFlowReactive[P, V, R]] extends FlowOps { this: R =>

  }

  trait SimpleFlowReactive[P, V, R <: SimpleFlowReactive[P, V, R]] extends OpaqueReactive[P, V]
    with FlowNode
    with SimpleReactive[P, V]
    with SimpleFlowOps[P, V, R] { this: R =>
    override protected[react] def emit(p: Any) = super.emit(p)
    def react() { continue() }
  }

  trait SignalFlowOps[A] extends SimpleFlowOps[A, A, FlowSignal[A]] { this: FlowSignal[A] =>
    def update(a: A): Unit @suspendable = emit(a)
    def previous = getValue
  }

  trait EventsFlowOps[A] extends SimpleFlowOps[A, Unit, FlowEvents[A]] { this: FlowEvents[A] =>
    def <<(a: A): Unit @suspendable = emit(a)
  }

  abstract class FlowSignal[A](init: A) extends Signal[A]
    with SimpleFlowReactive[A, A, FlowSignal[A]]
    with SignalFlowOps[A] {
    pulse = init
    tick()

    def toStrict: Signal[A] = this

    override def reactiveDescriptor = "FlowSignal"
  }

  abstract class FlowEvents[A] extends Events[A]
    with SimpleFlowReactive[A, Unit, FlowEvents[A]]
    with EventsFlowOps[A] {
    tick()
    override def reactiveDescriptor = "FlowEvents"
  }

  object Reactor {
    def flow[A](op: FlowOps => Unit @suspendable): Reactor = new Reactor {
      def body = op(this)
    }

    /**
     * Creates a flow event that runs through the given `op` repeatedly until disposed, e.g., by
     * calling `halt`.
     */
    def loop[A](op: FlowOps => Unit @suspendable):  Reactor = new Reactor {
      def body = while (!isDisposed) op(this)
    }
  }

  abstract class Reactor extends FlowNode with LeafNode with FlowOps {
    tick()
    def react() { continue() }
    override def reactiveDescriptor = "Reactor"
  }
}