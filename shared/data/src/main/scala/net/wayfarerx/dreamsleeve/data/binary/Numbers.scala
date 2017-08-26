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

import net.wayfarerx.dreamsleeve.io._

/**
 * Mix in for the number value factory that supports binary IO operations.
 */
trait Numbers extends Factory[Value.Number] {

  /* Return the number value binary support object. */
  final override protected def binarySupport: Support[Value.Number] = Numbers

}

/**
 * Definitions associated with the number value binary IO operations.
 */
object Numbers extends Support[Value.Number] {

  /** The monad for reading the content of a number value record. */
  val contentReader: BinaryReader[Either[Problems.Reading, Value.Number]] =
    for (d <- readDouble()) yield Right(Value.Number(d))

  /* The monad for reading an entire number value record. */
  override val recordReader: BinaryReader[Either[Problems.Reading, Value.Number]] = for {
    b <- readByte()
    r <- b match {
      case Value.Number.Header => contentReader
      case h => report(Problems.InvalidHeader(Vector(Value.Number.Header), h))
    }
  } yield r

  /* Create a monad for writing the entire record for the specified number value. */
  override def recordWriter(number: Value.Number): BinaryWriter[Unit] = for {
    _ <- writeByte(Value.Number.Header)
    _ <- writeDouble(number.value)
  } yield ()

}
