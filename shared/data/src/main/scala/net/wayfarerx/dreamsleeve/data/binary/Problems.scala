/*
 * Problems.scala
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
package binary

import net.wayfarerx.dreamsleeve.io._

/**
 * Base type for all binary data problems.
 */
sealed trait Problems extends Problem

/**
 * Definitions of the possible binary data problems.
 */
object Problems extends Problem.Factory[Problems] {

  /**
   * Marker trait for problems that can occur during binary data input operations.
   */
  sealed trait Reading extends Problems

  /**
   * Marker trait for problems that can occur during binary data output operations.
   */
  sealed trait Writing extends Problems

  /**
   * Problem returned when an invalid header is encountered.
   *
   * @param expected The headers that were expected.
   * @param found    The invalid header that was encountered.
   */
  case class InvalidHeader(expected: Vector[Byte], found: Byte) extends Reading

  /**
   * Problem returned when IO operations fail.
   *
   * @param cause The IO problem that was encountered.
   */
  case class Failure(cause: IOProblem) extends Reading with Writing

}
