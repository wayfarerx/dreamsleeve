/*
 * package.scala
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

package net.wayfarerx.dreamsleeve.data

import language.implicitConversions

import cats.free.Free
import cats.implicits._

import net.wayfarerx.dreamsleeve.io._

/**
 * Global definitions for the binary data package.
 */
package object binary_data {

  import binary_data.Problems._

  /** A binary writer monad that returns unit. */
  private[binary_data] val writing: BinaryWriter[Unit] =
    Free.pure(Right(()))

  /**
   * Constructs a binary reader monad that returns a result.
   *
   * @param result The result to return.
   * @tparam T The type of the expected result.
   * @return A binary reader monad that returns a result.
   */
  private[binary_data] def reading[T](result: T): BinaryReader[Either[Reading, T]] =
    Free.pure(Right(result))

  /**
   * Constructs a binary reader monad that returns a problem.
   *
   * @param problem The problem to return.
   * @tparam T The type of the expected result.
   * @return A binary reader monad that returns a problem.
   */
  private[binary_data] def report[T](problem: Reading): BinaryReader[Either[Reading, T]] =
    Free.pure(Left(problem))

  /**
   * Base class for support objects that enable IO operations.
   *
   * @tparam T The type of data that is supported.
   */
  trait Support[T] {

    /**
     * Returns the monad for reading an entire record.
     *
     * @return The monad for reading an entire record.
     */
    def recordReader: BinaryReader[Either[Reading, T]]

    /**
     * Creates a monad for writing the entire record for the specified data.
     *
     * @param data The data to create a writer for.
     * @return A monad for writing the entire record for the specified data.
     */
    def recordWriter(data: T): BinaryWriter[Unit]

  }

  /**
   * Base class for factory mix ins that support binary IO operations.
   *
   * @tparam T The type of data object managed by this factory.
   */
  trait Factory[T] {

    /**
     * Wraps a data item with extensions that support binary IO operations.
     *
     * @param data The data item to extend.
     * @return The specified data item wrapped with extensions that support binary IO operations.
     */
    final implicit def dataToBinaryExtensions(data: T): Extensions =
      new Extensions(binarySupport.recordWriter(data))

    /**
     * Reads a boolean value record from the specified binary input.
     *
     * @param input The binary input to read from.
     * @return The boolean value that was read or any problem that was encountered.
     */
    final def fromBytes(input: BinaryInput): Either[Reading, T] =
      binarySupport.recordReader.apply(input).left.map(Failure(_): Reading).flatten

    /**
     * Returns a binary reader for the underlying data type.
     *
     * @return A binary reader for the underlying data type.
     */
    protected def binarySupport: Support[T]

  }

  /**
   * Extensions that add support for binary IO operations.
   *
   * @param writer The writer to extend.
   */
  final class Extensions private[binary_data](val writer: BinaryWriter[Unit]) extends AnyVal {

    /**
     * Writes the value record to the specified binary output.
     *
     * @param output The binary output to write to.
     * @return Any problem that was encountered.
     */
    def toBytes(output: BinaryOutput): Either[Writing, Unit] =
      writer(output).left.map(Failure(_))

  }

}
