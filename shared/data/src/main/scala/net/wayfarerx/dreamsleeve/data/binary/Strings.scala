/*
 * Strings.scala
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
 * Mix in for the string value factory that supports binary IO operations.
 */
trait Strings {

  import Strings._

  /**
   * Wraps a string value with extensions that support binary IO operations.
   *
   * @param string The string value to extend.
   * @return The specified string value wrapped with extensions that support binary IO operations.
   */
  final implicit def stringToBinaryExtensions(string: Value.String): Extensions =
    new Extensions(recordWriter(string))

  /**
   * Reads a string value record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The string value that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Value.String] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the string value binary IO operations.
 */
object Strings {

  /** The monad for reading the content of a string value record. */
  val ContentReader: BinaryReader[Either[Problems.Reading, Value.String]] =
    for (s <- readString()) yield Right(Value.String(s.toString))

  /** The monad for reading an entire string value record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Value.String]] = for {
    b <- readByte()
    r <- b match {
      case Value.String.Header => ContentReader
      case h => report[Value.String](InvalidHeader(Vector(Value.String.Header), h))
    }
  } yield r

  /**
   * Creates a monad for writing the entire record for the specified string value.
   *
   * @param string The string value to create a writer for.
   * @return A monad for writing the entire record for the specified string value.
   */
  def recordWriter(string: Value.String): BinaryWriter[Unit] = for {
    _ <- writeByte(Value.String.Header)
    _ <- writeString(string.value)
  } yield ()

}
