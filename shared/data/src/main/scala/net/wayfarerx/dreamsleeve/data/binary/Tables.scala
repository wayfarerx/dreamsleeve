/*
 * Tables.scala
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
 * Mix in for the table factory that supports binary IO operations.
 */
trait Tables {

  import Tables._

  /**
   * Reads a table record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The table that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Table] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the table binary IO operations.
 */
object Tables {

  /** The monad for reading the content of a table record. */
  val ContentReader: BinaryReader[Either[Problems.Reading, Table]] = for {
    c <- readInt()
    e <- (pureRead(Vector[(Value, Fragment)]()) /: (0 until c)) { (r, _) =>
      for {
        e <- r
        k <- Values.RecordReader
        v <- Fragments.RecordReader
      } yield for {
        ee <- e
        kk <- k
        vv <- v
      } yield ee :+ kk -> vv
    }
  } yield for (ee <- e) yield Table(ee: _*)

  /** The monad for reading an entire table record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Table]] = for {
    b <- readByte()
    r <- b match {
      case Table.Header => ContentReader
      case h => report[Table](InvalidHeader(Vector(Table.Header), h))
    }
  } yield r

}
