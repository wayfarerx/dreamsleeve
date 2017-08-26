/*
 * Documents.scala
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
 * Mix in for the document factory that supports binary IO operations.
 */
trait Documents {

  import Documents._

  /**
   * Reads a document record from the specified binary input.
   *
   * @param input The binary input to read from.
   * @return The document that was read or any problem that was encountered.
   */
  final def fromBytes(input: BinaryInput): Either[Problems.Reading, Document] =
    RecordReader(input).left.map(Failure(_): Problems.Reading).flatten

}

/**
 * Definitions associated with the document binary IO operations.
 */
object Documents {

  /** The monad for reading the content of a document record. */
  val ContentReader: BinaryReader[Either[Problems.Reading, Document]] = for {
    t <- readString()
    c <- Fragments.RecordReader
  } yield for (cc <- c) yield Document(t.toString, cc)

  /** The monad for reading an entire document record. */
  val RecordReader: BinaryReader[Either[Problems.Reading, Document]] = for {
    b <- readByte()
    r <- b match {
      case Document.Header => ContentReader
      case h => report[Document](InvalidHeader(Vector(Document.Header), h))
    }
  } yield r

}
