package scala.react.monitor

import java.io.File
import java.nio.channels.FileChannel
import java.nio.BufferOverflowException

/**
 * A writer with a MappedByteBuffer-like interface that can be used to write to memory mapped files
 * for which the size is unknown in advance. The file size is increased by `chunkSize` (plus/minus
 * alignment) everytime it runs out of space.
 */
class PagedWriter(file: File, chunkSize: Int = 1000000) {
  private var offset = 0L
  val channel = IO.writeChannel(file)
  private var buf = IO.mapForWriting(channel, offset, chunkSize)

  private def remap() {
    offset += buf.position
    buf = channel.map(FileChannel.MapMode.READ_WRITE, offset, chunkSize)
  }

  def putLong(v: Long): Unit = try {
    buf putLong v
  } catch {
    case e: BufferOverflowException =>
      remap()
      putLong(v)
  }

  def putByte(v: Byte): Unit = try {
    buf put v
  } catch {
    case e: BufferOverflowException =>
      remap()
      putByte(v)
  }

  def putShort(v: Short): Unit = try {
    buf putShort v
  } catch {
    case e: BufferOverflowException =>
      remap()
      putShort(v)
  }

  def putInt(v: Int): Unit = try {
    buf putInt v
  } catch {
    case e: BufferOverflowException =>
      remap()
      putInt(v)
  }
}