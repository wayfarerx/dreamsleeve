/*
 * IOProblem.scala
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

package net.wayfarerx.dreamsleeve.io

import java.io.IOException
import java.nio.charset.CharacterCodingException

/**
 * Base type for all IO problems.
 */
sealed trait IOProblem

/**
 * Definitions of the possible IO problems.
 */
object IOProblem {

  /**
   * Marker trait for problems that can occur during input operations.
   */
  trait Reading extends IOProblem

  /**
   * Marker trait for problems that can occur during output operations.
   */
  trait Writing extends IOProblem

  /**
   * Problem returned when reaching end-of-file while reading.
   */
  case object Underflow extends Reading

  /**
   * Problem returned when reaching end-of-file while writing.
   */
  case object Overflow extends Writing

  /**
   * Problem returned when character decoding operations fail.
   *
   * @param thrown The exception that was thrown.
   */
  case class Decoding(thrown: CharacterCodingException) extends Reading

  /**
   * Problem returned when character encoding operations fail.
   *
   * @param thrown The exception that was thrown.
   */
  case class Encoding(thrown: CharacterCodingException) extends Writing

  /**
   * Problem returned when IO operations fail.
   *
   * @param thrown The exception that was thrown.
   */
  case class Failure(thrown: IOException) extends Reading with Writing

}

object IOTEST {

  def main(args: Array[String]): Unit = {
    val writer = for {
      _ <- writeByte(3)
      _ <- writeShort(5)
      _ <- writeInt(7)
      _ <- writeLong(11)
    } yield ()

    val reader = for {
      a <- readByte()
      b <- readShort()
      c <- readInt()
      d <- readLong()
    } yield (a, b, c, d)

    val baos = new java.io.ByteArrayOutputStream()
    writer(baos)
    println(reader(baos.toByteArray))
  }

}