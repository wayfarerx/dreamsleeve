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

package net.wayfarerx.dreamsleeve
package io

import language.implicitConversions

import java.io.{IOException, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, WritableByteChannel}

import cats.implicits._


/**
 * Base class for the binary output type.
 */
trait BinaryOutput {

  /**
   * Attempts to write bytes to the output, emptying as much of the supplied buffer as possible.
   *
   * @param bytes The buffer to write bytes from.
   * @return The number of bytes written or any problem that was encountered.
   */
  def write(bytes: ByteBuffer): IOResult.Output[Int]

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
  def apply(output: WritableByteChannel): BinaryOutput = (bytes: ByteBuffer) =>
    Either.catchOnly[IOException](output.write(bytes)).left.map(IOProblem.Failure)

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
  def apply(output: ByteBuffer): BinaryOutput = (bytes: ByteBuffer) =>
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
