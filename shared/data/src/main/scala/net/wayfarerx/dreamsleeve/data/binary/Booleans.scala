/*
 * Booleans.scala
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
 * Mix in for the boolean value factory that supports binary IO operations.
 */
trait Booleans {

  import Booleans._

  /**
   * Wraps a boolean value with extensions that support binary IO operations.
   *
   * @param boolean The boolean value to extend.
   * @return The specified boolean value wrapped with extensions that support binary IO operations.
   */
  final implicit def booleanToBinaryExtensions(boolean: Value.Boolean): Extensions =
    new Extensions(recordWriter(boolean))

  /**
   * Reads a boolean value record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The boolean value that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Value.Boolean] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the boolean value binary IO operations.
 */
object Booleans {

  /** The monad for reading the content of a boolean value record. */
  val ContentReader: BinaryReader[Either[Problems.Reading, Value.Boolean]] =
    for (b <- readByte()) yield Right(Value.Boolean(b != 0))

  /** The monad for reading an entire boolean value record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Value.Boolean]] = for {
    b <- readByte()
    r <- b match {
      case Value.Boolean.Header => ContentReader
      case h => report[Value.Boolean](InvalidHeader(Vector(Value.Boolean.Header), h))
    }
  } yield r

  /**
   * Creates a monad for writing the entire record for the specified boolean value.
   *
   * @param boolean The boolean value to create a writer for.
   * @return A monad for writing the entire record for the specified boolean value.
   */
  def recordWriter(boolean: Value.Boolean): BinaryWriter[Unit] = for {
    _ <- writeByte(Value.Boolean.Header)
    _ <- writeByte(if (boolean.value) 1 else 0)
  } yield ()

}
