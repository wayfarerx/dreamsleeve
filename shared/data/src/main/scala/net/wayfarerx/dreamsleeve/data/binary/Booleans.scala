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

import net.wayfarerx.dreamsleeve.io._

/**
 * Mix in for the boolean value factory that supports binary IO operations.
 */
trait Booleans extends Factory[Value.Boolean] {

  /* Return the boolean value binary support object. */
  final override protected def binarySupport: Support[Value.Boolean] = Booleans

}

/**
 * Definitions associated with the boolean value binary IO operations.
 */
object Booleans extends Support[Value.Boolean] {

  /** The monad for reading the content of a boolean value record. */
  val contentReader: BinaryReader[Either[Problems.Reading, Value.Boolean]] =
    for (b <- readByte()) yield Right(Value.Boolean(b != 0))

  /* The monad for reading an entire boolean value record. */
  override val recordReader: BinaryReader[Either[Problems.Reading, Value.Boolean]] = for {
    b <- readByte()
    r <- b match {
      case Value.Boolean.Header => contentReader
      case h => report(Problems.InvalidHeader(Vector(Value.Boolean.Header), h))
    }
  } yield r

  /* Create a monad for writing the entire record for the specified boolean value. */
  override def recordWriter(boolean: Value.Boolean): BinaryWriter[Unit] = for {
    _ <- writeByte(Value.Boolean.Header)
    _ <- writeByte(if (boolean.value) 1 else 0)
  } yield ()

}
