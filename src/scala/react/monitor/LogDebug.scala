package scala.react
package monitor

import java.io._
import java.nio.MappedByteBuffer
import java.nio.BufferOverflowException
import java.nio.channels.FileChannel
import java.util.WeakHashMap
import scala.collection.mutable.ArrayBuffer

class LogDebug[D <: Domain](dom: D) extends AbstractDebug(dom) {
  import domain._
  import LogFormat._

  private val log = new PagedWriter(IO.newLogFile)
  private val header = IO.mapForWriting(log.channel, 0, 8)

  private val ids = new WeakHashMap[Node, Long]
  private var idCounter = 0L // 0 is reserved
  private var idSize = 1.toByte // in bytes
  private def uniqueId(n: Node): Long = {
    var id = ids.get(n)
    if (id == 0) {
      idCounter += 1
      id = idCounter
      ids.put(n, id)

      if (id <= Byte.MaxValue) {
        writeTag(NewNodeTag)
        log putByte id.toByte
      } else if (id <= Short.MaxValue) {
        if (idSize == Byte.MaxValue + 1) {
          idSize = 2
          writeTag(NewNodeGenTag)
          log putByte idSize
        }
        writeTag(NewNodeTag)
        log putShort id.toShort
      } else if (id <= Int.MaxValue) {
        if (idSize == Short.MaxValue + 1) {
          idSize = 4
          writeTag(NewNodeGenTag)
          log putByte idSize
        }
        writeTag(NewNodeTag)
        log putInt id.toInt
      } else {
        if (idSize == Int.MaxValue + 1) {
          idSize = 8
          writeTag(NewNodeGenTag)
          log putByte idSize
        }
        writeTag(NewNodeTag)
        log putLong id
      }
    }
    id
  }

  private def writeTag(tag: Byte) {
    log putByte tag
  }

  private def writeNodeId(id: Long) {
    idSize match {
      case 1 => log putByte id.toByte
      case 2 => log putShort id.toShort
      case 4 => log putInt id.toInt
      case 8 => log putLong id
    }
  }

  private var t0 = 0L
  private var t = 0L
  private var lastSnapshot = 0L

  private def time() = System.nanoTime - t0

  def logStart() {
    val startTime = System.currentTimeMillis
    t0 = time()
    writeHeader(0)
    log putLong startTime
  }

  def writeHeader(l: Long) {
    header.rewind()
    header putLong l
  }

  def logEnterTurn(id: Long) = {
    t = time()
    log putLong id
    log putLong t
  }
  def logLeaveTurn(id: Long) = {
    t = time()
    if (t - lastSnapshot > 1000000000) {
      lastSnapshot = t
      writeHeader(t)
    }
    writeTag(TodoTag)
    log putInt todoCounter
    todoCounter = 0
    writeTag(EndTurnTag)
    log putLong t
  }
  def logTock(n: Node) = {
    val id = uniqueId(n)
    writeTag(TockTag)
    writeNodeId(id)
  }
  def logLevelMismatch(accessor: Node, accessed: Node) = {
    writeTag(MismatchTag)
    writeNodeId(uniqueId(accessor))
    writeNodeId(uniqueId(accessed))
  }
  private var todoCounter = 0
  def logTodo(t: Any) = {
    todoCounter += 1
  }
}

