/*
 * BinaryInput.scala
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

import java.io.{IOException, InputStream}
import java.nio._
import channels.{Channels, ReadableByteChannel}

import cats.implicits._


/**
 * Base class for the binary input type.
 */
trait BinaryInput extends BinaryContext {

  /**
   * Attempts to read bytes from the underlying source, filling as much of the supplied buffer as possible.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes The buffer to read bytes into.
   * @return The number of bytes read or any problem that was encountered.
   */
  def apply(bytes: ByteBuffer): IOResult.Input[Int]

  /**
   * Reads the specified number of bytes from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of bytes to read.
   * @return A buffer containing the requested bytes or any problem that occurs.
   */
  final def readBytes(count: Int): IOResult.Input[ByteBuffer] = {

    @inline
    @tailrec
    def readFully(buffer: ByteBuffer): IOResult.Input[ByteBuffer] = {
      apply(buffer) map { outcome =>
        if (outcome == 0) Some(Left(IOProblem.Underflow))
        else if (buffer.hasRemaining) None
        else Some(Right(buffer))
      } match {
        case Left(problem) => Left(problem)
        case Right(Some(result)) => result
        case Right(None) => readFully(buffer)
      }
    }

    readFully(acquireBytes(count)) map { buffer => buffer.rewind(); buffer }
  }

  /**
   * Reads the specified number of shorts from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of shorts to read.
   * @return A buffer containing the requested shorts or any problem that occurs.
   */
  final def readShorts(count: Int): IOResult.Input[ShortBuffer] =
    readBytes(count * 2) map (_.asShortBuffer())

  /**
   * Reads the specified number of chars from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of chars to read.
   * @return A buffer containing the requested chars or any problem that occurs.
   */
  final def readChars(count: Int): IOResult.Input[CharBuffer] =
    readBytes(count * 2) map (_.asCharBuffer())

  /**
   * Reads the specified number of ints from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of ints to read.
   * @return A buffer containing the requested ints or any problem that occurs.
   */
  final def readInts(count: Int): IOResult.Input[IntBuffer] =
    readBytes(count * 4) map (_.asIntBuffer())

  /**
   * Reads the specified number of floats from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of floats to read.
   * @return A buffer containing the requested floats or any problem that occurs.
   */
  final def readFloats(count: Int): IOResult.Input[FloatBuffer] =
    readBytes(count * 4) map (_.asFloatBuffer())

  /**
   * Reads the specified number of longs from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of longs to read.
   * @return A buffer containing the requested longs or any problem that occurs.
   */
  final def readLongs(count: Int): IOResult.Input[LongBuffer] =
    readBytes(count * 8) map (_.asLongBuffer())

  /**
   * Reads the specified number of doubles from the input, returning an underflow problem if there are not enough.
   *
   * WARNING: The buffer returned by this method may be overwritten in subsequent calls to this object.
   *
   * @param count The number of doubles to read.
   * @return A buffer containing the requested doubles or any problem that occurs.
   */
  final def readDoubles(count: Int): IOResult.Input[DoubleBuffer] =
    readBytes(count * 8) map (_.asDoubleBuffer())

}

/**
 * Common implementations of the binary input type.
 */
object BinaryInput {

  /** Implicit binary input support for bytes. */
  @inline
  implicit def byteToBinaryInput(input: Byte): BinaryInput = apply(input)

  /** Implicit binary input support for byte arrays. */
  @inline
  implicit def byteArrayToBinaryInput(input: Array[Byte]): BinaryInput = apply(input)

  /** Implicit binary input support for byte buffers. */
  @inline
  implicit def byteBufferToBinaryInput(input: ByteBuffer): BinaryInput = apply(input)

  /** Implicit binary input support for input streams. */
  @inline
  implicit def inputStreamToBinaryInput(input: InputStream): BinaryInput = apply(input)

  /** Implicit binary input support for readable byte channels. */
  @inline
  implicit def readableByteChannelToBinaryInput(input: ReadableByteChannel): BinaryInput = apply(input)

  /**
   * Creates a binary input implementation for the specified byte.
   *
   * @param input The byte to wrap with the binary input type.
   */
  @inline
  def apply(input: Byte): BinaryInput =
  apply(Array[Byte](input))

  /**
   * Creates a binary input implementation for the specified byte array.
   *
   * @param input The byte array to wrap with the binary input type.
   */
  @inline
  def apply(input: Array[Byte]): BinaryInput =
    apply(input, 0)

  /**
   * Creates a binary input implementation for the specified byte array.
   *
   * @param input  The byte array to wrap with the binary input type.
   * @param offset The offset into the array to start reading at.
   */
  @inline
  def apply(input: Array[Byte], offset: Int): BinaryInput =
    apply(input, offset, input.length - offset)

  /**
   * Creates a binary input implementation for the specified byte array.
   *
   * @param input  The byte array to wrap with the binary input type.
   * @param offset The offset into the array to start reading at.
   * @param count  The maximum number of bytes that can be read.
   */
  @inline
  def apply(input: Array[Byte], offset: Int, count: Int): BinaryInput =
    apply(ByteBuffer.wrap(input, offset, count))

  /**
   * Creates a binary input implementation for the specified byte buffer.
   *
   * @param input The byte buffer to wrap with the binary input type.
   */
  def apply(input: ByteBuffer): BinaryInput = new BinaryContext.Support with BinaryInput {
    override def apply(bytes: ByteBuffer): IOResult.Input[Int] =
      if (!input.hasRemaining) Left(IOProblem.Underflow)
      else {
        val length = Math.min(bytes.remaining(), input.remaining())
        if (input.remaining() <= length) bytes.put(input) else {
          val old = input.limit()
          input.limit(old - input.remaining() + length)
          bytes.put(input)
          input.limit(old)
        }
        Right(length)
      }
  }

  /**
   * Creates a binary input implementation for the specified input stream.
   *
   * @param input The input stream to wrap with the binary input type.
   */
  @inline
  def apply(input: InputStream): BinaryInput =
  apply(Channels.newChannel(input))

  /**
   * Creates a binary input implementation for the specified readable byte channel.
   *
   * @param input The readable byte channel to wrap with the binary input type.
   */
  def apply(input: ReadableByteChannel): BinaryInput = new BinaryContext.Support with BinaryInput {
    override def apply(bytes: ByteBuffer): IOResult.Input[Int] =
      Either.catchOnly[IOException](input.read(bytes)).left.map(IOProblem.Failure) flatMap
        (length => if (length < 0) Left(IOProblem.Underflow) else Right(length))
  }

}
