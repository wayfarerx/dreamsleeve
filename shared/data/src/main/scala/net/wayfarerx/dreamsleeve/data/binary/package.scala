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
   * Constructs a binary reader monad that returns a problem.
   *
   * @param problem The problem to return.
   * @tparam T The type of the expected result.
   * @return A binary reader monad that returns a problem.
   */
  private[binary] def report[T](problem: Reading): BinaryReader[Either[Reading, T]] =
    Free.pure(Left(problem))

  /**
   * Constructs a binary reader monad that returns a result.
   *
   * @param result The result to return.
   * @tparam T The type of the expected result.
   * @return A binary reader monad that returns a result.
   */
  private[binary] def pureRead[T](result: T): BinaryReader[Either[Reading, T]] =
    Free.pure(Right(result))

}
