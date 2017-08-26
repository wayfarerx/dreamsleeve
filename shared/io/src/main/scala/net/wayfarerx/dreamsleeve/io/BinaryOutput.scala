/*
 * BinaryOutput.scala
 *
 * Copyright 2017 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.wayfarerx.dreamsleeve.io


import annotation.tailrec
import language.implicitConversions

import java.io.{IOException, OutputStream}
import java.nio._
import channels.{Channels, WritableByteChannel}

import cats.implicits._


/**
 * Base class for the binary output type.
 */
trait BinaryOutput extends BinaryContext {

  /**
   * Attempts to write bytes to the output, emptying as much of the supplied buffer as possible.
   *
   * @param bytes The buffer to write bytes from.
   * @return The number of bytes written or any problem that was encountered.
   */
  def apply(bytes: ByteBuffer): IOResult.Output[Int]

  /**
   * Writes the specified number of bytes to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of bytes to write.
   * @param f     The function that writes the bytes.
   * @return Any problem that occurs.
   */
  final def writeBytes(count: Int)(f: ByteBuffer => Unit): IOResult.Output[Unit] = {

    @inline
    @tailrec
    def writeFully(buffer: ByteBuffer): IOResult.Output[Unit] =
      apply(buffer) map { outcome =>
        if (outcome == 0) Some(Left(IOProblem.Overflow))
        else if (buffer.hasRemaining) None
        else Some(Right(()))
      } match {
        case Left(problem) => Left(problem)
        case Right(Some(result)) => result
        case Right(None) => writeFully(buffer)
      }

    val buffer = acquireBytes(count)
    f(buffer)
    buffer.rewind()
    writeFully(buffer)
  }

  /**
   * Writes the specified number of shorts to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of shorts to write.
   * @param f     The function that writes the shorts.
   * @return Any problem that occurs.
   */
  final def writeShorts(count: Int)(f: ShortBuffer => Unit): IOResult.Output[Unit] =
    writeBytes(count * 2)(f.compose(_.asShortBuffer()))

  /**
   * Writes the specified number of chars to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of chars to write.
   * @param f     The function that writes the chars.
   * @return Any problem that occurs.
   */
  final def writeChars(count: Int)(f: CharBuffer => Unit): IOResult.Output[Unit] =
    writeBytes(count * 2)(f.compose(_.asCharBuffer()))

  /**
   * Writes the specified number of ints to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of ints to write.
   * @param f     The function that writes the ints.
   * @return Any problem that occurs.
   */
  final def writeInts(count: Int)(f: IntBuffer => Unit): IOResult.Output[Unit] =
    writeBytes(count * 4)(f.compose(_.asIntBuffer()))

  /**
   * Writes the specified number of floats to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of floats to write.
   * @param f     The function that writes the floats.
   * @return Any problem that occurs.
   */
  final def writeFloats(count: Int)(f: FloatBuffer => Unit): IOResult.Output[Unit] =
    writeBytes(count * 4)(f.compose(_.asFloatBuffer()))

  /**
   * Writes the specified number of longs to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of longs to write.
   * @param f     The function that writes the longs.
   * @return Any problem that occurs.
   */
  final def writeLongs(count: Int)(f: LongBuffer => Unit): IOResult.Output[Unit] =
    writeBytes(count * 8)(f.compose(_.asLongBuffer()))

  /**
   * Writes the specified number of doubles to the output, returning an overflow problem if there are too many.
   *
   * WARNING: The buffer passed to the supplied function may be overwritten in subsequent calls to this object.
   *
   * @param count The number of doubles to write.
   * @param f     The function that writes the doubles.
   * @return Any problem that occurs.
   */
  final def writeDoubles(count: Int)(f: DoubleBuffer => Unit): IOResult.Output[Unit] =
    writeBytes(count * 8)(f.compose(_.asDoubleBuffer()))

}

/**
 * Common implementations of the binary output type.
 */
object BinaryOutput {

  /** Implicit binary output support for output streams. */
  @inline
  implicit def outputStreamToBinaryOutput(output: OutputStream): BinaryOutput = apply(output)

  /** Implicit binary output support for writable byte channels. */
  @inline
  implicit def writableByteChannelToBinaryOutput(output: WritableByteChannel): BinaryOutput = apply(output)

  /** Implicit binary output support for byte arrays. */
  @inline
  implicit def byteArrayToBinaryOutput(output: Array[Byte]): BinaryOutput = apply(output)

  /** Implicit binary output support for byte buffers. */
  @inline
  implicit def byteBufferToBinaryOutput(output: ByteBuffer): BinaryOutput = apply(output)

  /**
   * Creates a binary output implementation for the specified output stream.
   *
   * @param output The output stream to wrap with the binary output type.
   */
  def apply(output: OutputStream): BinaryOutput =
    apply(Channels.newChannel(output))

  /**
   * Creates a binary output implementation for the specified writable byte channel.
   *
   * @param output The writable byte channel to wrap with the binary output type.
   */
  def apply(output: WritableByteChannel): BinaryOutput = new BinaryContext.Support with BinaryOutput {
    override def apply(bytes: ByteBuffer): IOResult.Output[Int] =
      Either.catchOnly[IOException](output.write(bytes)).left.map(IOProblem.Failure)
  }

  /**
   * Creates a binary output implementation for the specified byte array.
   *
   * @param output The byte array to wrap with the binary output type.
   */
  def apply(output: Array[Byte]): BinaryOutput =
    apply(output, 0)

  /**
   * Creates a binary output implementation for the specified byte array.
   *
   * @param output The byte array to wrap with the binary output type.
   * @param offset The offset into the array to start writing at.
   */
  def apply(output: Array[Byte], offset: Int): BinaryOutput =
    apply(output, offset, output.length - offset)

  /**
   * Creates a binary output implementation for the specified byte array.
   *
   * @param output The byte array to wrap with the binary output type.
   * @param offset The offset into the array to start writing at.
   * @param count  The maximum number of bytes that can be write.
   */
  def apply(output: Array[Byte], offset: Int, count: Int): BinaryOutput =
    apply(ByteBuffer.wrap(output, offset, count))

  /**
   * Creates a binary output implementation for the specified byte buffer.
   *
   * @param output The byte buffer to wrap with the binary output type.
   */
  def apply(output: ByteBuffer): BinaryOutput = new BinaryContext.Support with BinaryOutput {
    override def apply(bytes: ByteBuffer): IOResult.Output[Int] =
      if (!output.hasRemaining) Left(IOProblem.Overflow)
      else {
        val length = Math.min(bytes.remaining(), output.remaining())
        if (bytes.remaining() <= length) output.put(bytes) else {
          val old = bytes.limit()
          bytes.limit(old - bytes.remaining() + length)
          output.put(bytes)
          bytes.limit(old)
        }
        Right(length)
      }
  }

}
