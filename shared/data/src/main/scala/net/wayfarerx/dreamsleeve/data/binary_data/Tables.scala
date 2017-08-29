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
package binary_data

import net.wayfarerx.dreamsleeve.io._

/**
 * Mix in for the table factory that supports binary IO operations.
 */
trait Tables extends Factory[Table] {

  /* Return the fragment binary support object. */
  final override protected def binarySupport: Support[Table] = Tables

}

/**
 * Definitions associated with the table binary IO operations.
 */
object Tables extends Support[Table] {

  /** The monad for reading the content of a table record. */
  val contentReader: BinaryReader[Either[Problems.Reading, Table]] = for {
    c <- readInt()
    e <- (reading(Vector[(Value, Fragment)]()) /: (0 until c)) { (r, _) =>
      for {
        i <- r
        k <- Values.recordReader
        v <- Fragments.recordReader
      } yield for {
        ii <- i
        kk <- k
        vv <- v
      } yield ii :+ kk -> vv
    }
  } yield for (ee <- e) yield Table(ee: _*)

  /* The monad for reading an entire table record. */
  override val recordReader: BinaryReader[Either[Problems.Reading, Table]] = for {
    b <- readByte()
    r <- b match {
      case Table.Header => contentReader
      case h => report(Problems.InvalidHeader(Vector(Table.Header), h))
    }
  } yield r

  /* Create a monad for writing the entire record for the specified table. */
  override def recordWriter(table: Table): BinaryWriter[Unit] = for {
    _ <- writeByte(Table.Header)
    _ <- writeInt(table.entries.size)
    _ <- (writing /: table.entries) { (p, e) =>
      val (k, v) = e
      for {
        _ <- p
        _ <- Values.recordWriter(k)
        _ <- Fragments.recordWriter(v)
      } yield ()
    }
  } yield ()

}
