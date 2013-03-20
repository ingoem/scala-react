package scala.react.monitor

import java.io._
import java.nio.channels.FileChannel
import java.nio.{ByteBuffer, MappedByteBuffer}

object IO {
  def readChannel(file: File): FileChannel = new RandomAccessFile(file, "r").getChannel
  def writeChannel(file: File): FileChannel = new RandomAccessFile(file, "rw").getChannel

  def mapForReading(ch: FileChannel, offset: Long, size: Int): MappedByteBuffer =
    ch.map(FileChannel.MapMode.READ_ONLY, offset, size)

  def mapForWriting(ch: FileChannel, offset: Long, size: Int): MappedByteBuffer =
    ch.map(FileChannel.MapMode.READ_WRITE, offset, size)

  def newLogFile: File = File.createTempFile("scalareact", "log")

  def unmap(buf: ByteBuffer) {
    buf match {
      case buf : sun.nio.ch.DirectBuffer =>
        buf.cleaner.clean()
      case _ =>
        throw new RuntimeException("Cannot unmap given buffer")
    }
  }
}