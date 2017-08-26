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

import net.wayfarerx.dreamsleeve.io._

/**
 * Mix in for the document factory that supports binary IO operations.
 */
trait Documents extends Factory[Document] {

  /* Return the fragment binary support object. */
  final override protected def binarySupport: Support[Document] = Documents

}

/**
 * Definitions associated with the document binary IO operations.
 */
object Documents extends Support[Document] {

  /** The monad for reading the content of a document record. */
  val contentReader: BinaryReader[Either[Problems.Reading, Document]] = for {
    t <- readString()
    c <- Fragments.recordReader
  } yield for (cc <- c) yield Document(t.toString, cc)

  /* The monad for reading an entire document record. */
  override val recordReader: BinaryReader[Either[Problems.Reading, Document]] = for {
    b <- readByte()
    r <- b match {
      case Document.Header => contentReader
      case h => report(Problems.InvalidHeader(Vector(Document.Header), h))
    }
  } yield r

  /* Create a monad for writing the entire record for the specified document. */
  override def recordWriter(document: Document): BinaryWriter[Unit] = for {
    _ <- writeByte(Document.Header)
    _ <- writeString(document.title)
    _ <- Fragments.recordWriter(document.content)
  } yield ()

}
