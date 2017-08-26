/*
 * Numbers.scala
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

import language.implicitConversions

import cats.implicits._

import net.wayfarerx.dreamsleeve.io._
import Problems._

/**
 * Mix in for the number value factory that supports binary IO operations.
 */
trait Numbers {

  import Numbers._

  /**
   * Wraps a number value with extensions that support binary IO operations.
   *
   * @param number The number value to extend.
   * @return The specified number value wrapped with extensions that support binary IO operations.
   */
  final implicit def numberToBinaryExtensions(number: Value.Number): Extensions =
    new Extensions(recordWriter(number))

  /**
   * Reads a number value record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The number value that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Value.Number] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the number value binary IO operations.
 */
object Numbers {

  /** The monad for reading the content of a number value record. */
  val ContentReader: BinaryReader[Either[Problems.Reading, Value.Number]] =
    for (d <- readDouble()) yield Right(Value.Number(d))

  /** The monad for reading an entire number value record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Value.Number]] = for {
    b <- readByte()
    r <- b match {
      case Value.Number.Header => ContentReader
      case h => report[Value.Number](InvalidHeader(Vector(Value.Number.Header), h))
    }
  } yield r

  /**
   * Creates a monad for writing the entire record for the specified number value.
   *
   * @param number The number value to create a writer for.
   * @return A monad for writing the entire record for the specified number value.
   */
  def recordWriter(number: Value.Number): BinaryWriter[Unit] = for {
    _ <- writeByte(Value.Number.Header)
    _ <- writeDouble(number.value)
  } yield ()

}
