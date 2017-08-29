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
package binary_data

import net.wayfarerx.dreamsleeve.io._

/**
 * Mix in for the fragment factory that supports binary IO operations.
 */
trait Fragments extends Factory[Fragment] {

  /* Return the fragment binary support object. */
  final override protected def binarySupport: Support[Fragment] = Fragments

}

/**
 * Definitions associated with the fragment binary IO operations.
 */
object Fragments extends Support[Fragment] {

  /* The monad for reading an entire fragment record. */
  override val recordReader: BinaryReader[Either[Problems.Reading, Fragment]] = for {
    b <- readByte()
    r <- b match {
      case Value.Boolean.Header => Booleans.contentReader
      case Value.Number.Header => Numbers.contentReader
      case Value.String.Header => Strings.contentReader
      case Table.Header => Tables.contentReader
      case h => report(
        Problems.InvalidHeader(Vector(Value.Boolean.Header, Value.Number.Header, Value.String.Header, Table.Header), h))
    }
  } yield r

  /* Create a monad for writing the entire record for the specified fragment. */
  override def recordWriter(fragment: Fragment): BinaryWriter[Unit] = fragment match {
    case v@Value() => Values.recordWriter(v)
    case t@Table(_) => Tables.recordWriter(t)
  }

}
