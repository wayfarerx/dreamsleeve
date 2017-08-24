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

package net.wayfarerx.dreamsleeve

import cats.Monad

/**
 * The interface that enables functional & extensible IO operations.
 */
package object io extends io.BinaryIO {

  /** The type of operations that read binary data. */
  type BinaryReader[T] = BinaryIO.BinaryReader[T]

  /** The type of operations that write binary data. */
  type BinaryWriter[T] = BinaryIO.BinaryWriter[T]

  /** The type of result returned from IO operations. */
  type IOResult[P <: IOProblem, T] = Either[P, T]

  /**
   * Definitions of the input and output result types.
   */
  object IOResult {

    /** The type of result returned from input operations. */
    type Input[T] = IOResult[IOProblem.Reading, T]

    /** The type of result returned from output operations. */
    type Output[T] = IOResult[IOProblem.Writing, T]

  }

  /** The monad used for merging the results of input operations. */
  implicit val IOResultInputMonad: Monad[IOResult.Input] = new ProblematicMonad[IOProblem.Reading]

  /** The monad used for merging the results of input operations. */
  implicit val IOResultOutputMonad: Monad[IOResult.Output] = new ProblematicMonad[IOProblem.Writing]

}
