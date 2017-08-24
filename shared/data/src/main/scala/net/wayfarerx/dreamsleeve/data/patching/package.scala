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

import cats.data.Validated

/**
 * Global definitions for the patching package.
 */
package object patching {

  /** The type that represents the result of error-prone patching operations. */
  type Result[T] = Either[Vector[Problems], T]

  /** The internal representation of the result of error-prone patching operations. */
  private[patching] type Attempt[T] = Validated[patching.Problems.List, T]

  /**
   * Implicitly converts any attempt into a result.
   *
   * @tparam T The type of the underlying result.
   * @param attempt The attempt to convert.
   * @return The converted attempt.
   */
  @inline
  implicit private[patching] def attemptToResult[T](attempt: Attempt[T]): Result[T] =
  attempt.leftMap(_.toList.toVector).toEither

}
