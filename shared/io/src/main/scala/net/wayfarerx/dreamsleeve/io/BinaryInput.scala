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

package net.wayfarerx.dreamsleeve
package io

import language.implicitConversions

import java.io.{IOException, InputStream}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, ReadableByteChannel}

import cats.implicits._

/**
 * Base class for the binary input type.
 */
trait BinaryInput {

  /**
   * Attempts to read bytes from the underlying source, filling as much of the supplied buffer as possible.
   *
   * @param bytes The buffer to read bytes into.
   * @return The number of bytes read or any problem that was encountered.
   */
  def read(bytes: ByteBuffer): IOResult.Input[Int]

}

/**
 * Common implementations of the binary input type.
 */
object BinaryInput {

  /** Implicit binary input support for input streams. */
  @inline
  implicit def inputStreamToBinaryInput(input: InputStream): BinaryInput = apply(input)

  /** Implicit binary input support for readable byte channels. */
  @inline
  implicit def readableByteChannelToBinaryInput(input: ReadableByteChannel): BinaryInput = apply(input)

  /** Implicit binary input support for byte arrays. */
  @inline
  implicit def byteArrayToBinaryInput(input: Array[Byte]): BinaryInput = apply(input)

  /** Implicit binary input support for byte buffers. */
  @inline
  implicit def byteBufferToBinaryInput(input: ByteBuffer): BinaryInput = apply(input)

  /**
   * Creates a binary input implementation for the specified input stream.
   *
   * @param input The input stream to wrap with the binary input type.
   */
  def apply(input: InputStream): BinaryInput =
    apply(Channels.newChannel(input))

  /**
   * Creates a binary input implementation for the specified readable byte channel.
   *
   * @param input The readable byte channel to wrap with the binary input type.
   */
  def apply(input: ReadableByteChannel): BinaryInput = (bytes: ByteBuffer) =>
    Either.catchOnly[IOException](input.read(bytes)).left.map(IOProblem.Failure) flatMap
      (length => if (length < 0) Left(IOProblem.Underflow) else Right(length))

  /**
   * Creates a binary input implementation for the specified byte array.
   *
   * @param input The byte array to wrap with the binary input type.
   */
  def apply(input: Array[Byte]): BinaryInput =
    apply(input, 0)

  /**
   * Creates a binary input implementation for the specified byte array.
   *
   * @param input  The byte array to wrap with the binary input type.
   * @param offset The offset into the array to start reading at.
   */
  def apply(input: Array[Byte], offset: Int): BinaryInput =
    apply(input, offset, input.length - offset)

  /**
   * Creates a binary input implementation for the specified byte array.
   *
   * @param input  The byte array to wrap with the binary input type.
   * @param offset The offset into the array to start reading at.
   * @param count  The maximum number of bytes that can be read.
   */
  def apply(input: Array[Byte], offset: Int, count: Int): BinaryInput =
    apply(ByteBuffer.wrap(input, offset, count))

  /**
   * Creates a binary input implementation for the specified byte buffer.
   *
   * @param input The byte buffer to wrap with the binary input type.
   */
  def apply(input: ByteBuffer): BinaryInput = (bytes: ByteBuffer) =>
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
