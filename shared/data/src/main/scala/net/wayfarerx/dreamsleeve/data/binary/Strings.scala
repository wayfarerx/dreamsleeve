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

import net.wayfarerx.dreamsleeve.io._

/**
 * Mix in for the string value factory that supports binary IO operations.
 */
trait Strings extends Factory[Value.String] {

  /* Return the string value binary support object. */
  final override protected def binarySupport: Support[Value.String] = Strings

}

/**
 * Definitions associated with the string value binary IO operations.
 */
object Strings extends Support[Value.String] {

  /** The monad for reading the content of a string value record. */
  val contentReader: BinaryReader[Either[Problems.Reading, Value.String]] =
    for (s <- readString()) yield Right(Value.String(s.toString))

  /* The monad for reading an entire string value record. */
  override val recordReader: BinaryReader[Either[Problems.Reading, Value.String]] = for {
    b <- readByte()
    r <- b match {
      case Value.String.Header => contentReader
      case h => report(Problems.InvalidHeader(Vector(Value.String.Header), h))
    }
  } yield r

  /* Create a monad for writing the entire record for the specified string value. */
  override def recordWriter(string: Value.String): BinaryWriter[Unit] = for {
    _ <- writeByte(Value.String.Header)
    _ <- writeString(string.value)
  } yield ()

}
