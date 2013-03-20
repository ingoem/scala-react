package scala.react
import scala.annotation.elidable

/**
 * Defines all reactive classes by mixing in all reactive modules.
 *
 * Clients usually create an 'object myDomain extends Domain' and import it.
 *
 * Except the scheduler interface, no method in neither class is guaranteed to be thread-safe.
 */
abstract class Domain extends ReactiveModule
  with SignalModule
  with EventModule
  with FlowModule
  with SchedulerModule { domain =>

  protected val scheduler: Scheduler
  protected def engine: Engine

  val debug: Debug[this.type] =
    System.getProperty("scala.react.debug", "no").toLowerCase match {
      case "no" => new NilDebug[this.type](domain)
      case "print" => new ConsoleDebug[this.type](this)
      case "log" => new monitor.LogDebug[this.type](this)
    }

  /**
   * A reactive version of `scala.App`, running all initialization code on this domain.
   * Automatically starts the domain in the main method.
   *
   * @see scala.App
   * @see scala.DelayedInit
   */
  trait ReactiveApp extends App {
    def main() {}

    override def main(args: Array[String]) {
      domain schedule { super.main(args) }
      domain.start()
      main()
    }
  }

  /**
   * Starts processing events. Thread-safe.
   */
  def start() {
    debug.logStart()
    scheduler.start()
  }
}

