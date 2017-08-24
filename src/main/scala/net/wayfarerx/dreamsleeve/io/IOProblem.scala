package net.wayfarerx.dreamsleeve
package io

import java.io.IOException


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