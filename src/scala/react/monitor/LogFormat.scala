package scala.react
package monitor

object LogFormat {
  val TockTag: Byte = 1
  val TodoTag: Byte = 2
  val MismatchTag: Byte = 3
  val NewNodeTag: Byte = 4
  val NewNodeGenTag: Byte = 5
  val EndTurnTag: Byte = -1
}
