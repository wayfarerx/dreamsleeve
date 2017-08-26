/*
 * Fragments.scala
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

import cats.implicits._

import net.wayfarerx.dreamsleeve.io._
import Problems._

/**
 * Mix in for the fragment factory that supports binary IO operations.
 */
trait Fragments {

  import Fragments._

  /**
   * Reads a fragment record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The fragment that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Fragment] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the fragment binary IO operations.
 */
object Fragments {

  /** The monad for reading an entire fragment record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Fragment]] = for {
    b <- readByte()
    r <- b match {
      case Value.Boolean.Header => Booleans.ContentReader
      case Value.Number.Header => Numbers.ContentReader
      case Value.String.Header => Strings.ContentReader
      case Table.Header => Tables.ContentReader
      case h => report[Fragment](
        InvalidHeader(Vector(Value.Boolean.Header, Value.Number.Header, Value.String.Header, Table.Header), h))
    }
  } yield r

}
