/*
 * Values.scala
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
 * Mix in for the value factory that supports binary IO operations.
 */
trait Values {

  import Values._

  /**
   * Reads a value record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The value that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Value] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the value binary IO operations.
 */
object Values {

  /** The monad for reading an entire value record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Value]] = for {
    b <- readByte()
    r <- b match {
      case Value.Boolean.Header => Booleans.ContentReader
      case Value.Number.Header => Numbers.ContentReader
      case Value.String.Header => Strings.ContentReader
      case h => report[Value](InvalidHeader(Vector(Value.Boolean.Header, Value.Number.Header, Value.String.Header), h))
    }
  } yield r

}
