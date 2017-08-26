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

import cats.free.Free

import net.wayfarerx.dreamsleeve.io._

/**
 * Global definitions for the binary data package.
 */
package object binary {

  import binary.Problems._

  /**
   * Constructs a binary writer monad that returns unit.
   *
   * @return A binary writer monad that returns unit.
   */
  private[binary] val WriteResult: BinaryWriter[Unit] =
    Free.pure(Right(()))

  /**
   * Constructs a binary reader monad that returns a result.
   *
   * @param result The result to return.
   * @tparam T The type of the expected result.
   * @return A binary reader monad that returns a result.
   */
  private[binary] def readResult[T](result: T): BinaryReader[Either[Reading, T]] =
    Free.pure(Right(result))

  /**
   * Constructs a binary reader monad that returns a problem.
   *
   * @param problem The problem to return.
   * @tparam T The type of the expected result.
   * @return A binary reader monad that returns a problem.
   */
  private[binary] def report[T](problem: Reading): BinaryReader[Either[Reading, T]] =
    Free.pure(Left(problem))

  /**
   * Extensions that add support for binary IO operations.
   *
   * @param writer The writer to extend.
   */
  final class Extensions private[binary](val writer: BinaryWriter[Unit]) extends AnyVal {

    /**
     * Writes the value record to the specified binary output.
     *
     * @param output The binary output to write to.
     * @return Any problem that was encountered.
     */
    def toBytes(output: BinaryOutput): Either[binary.Problems.Writing, Unit] =
      writer(output).left.map(Failure(_))

  }

}
