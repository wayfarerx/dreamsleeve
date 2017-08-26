/*
 * BinaryIO.scala
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

import language.implicitConversions

import java.nio._
import charset.{CharacterCodingException, Charset, StandardCharsets}

import cats._
import free.Free
import Free.liftF
import cats.implicits._

/**
 * Implementations of the common binary IO behavior.
 */
trait BinaryIO {

  import BinaryIO.{BinaryReaderExtensions, BinaryWriterExtensions, Read, Write}

  //
  // Implicit reader & writer support.
  //

  /**
   * Provides extensions to all instances of binary readers.
   *
   * @param reader The reader to provide extensions for.
   * @tparam T The type returned by the specified reader.
   * @return Extensions for the specified reader.
   */
  @inline
  final implicit def binaryReaderToBinaryReaderExtensions[T](reader: BinaryReader[T]): BinaryReaderExtensions[T] =
  new BinaryReaderExtensions(reader)

  /**
   * Provides extensions to all instances of binary writers.
   *
   * @param writer The writer to provide extensions for.
   * @tparam T The type returned by the specified writer.
   * @return Extensions for the specified writer.
   */
  @inline
  final implicit def binaryWriterToBinaryWriterExtensions[T](writer: BinaryWriter[T]): BinaryWriterExtensions[T] =
  new BinaryWriterExtensions(writer)

  //
  // Pure binary input operations.
  //

  /**
   * Reads a single byte from a binary input.
   *
   * @return A reader for a single byte.
   */
  final def readByte(): BinaryReader[Byte] =
    liftF[Read, Byte](_.readBytes(1) map (_.get()))

  /**
   * Reads the specified number of bytes from a binary input.
   *
   * @param count The number of bytes to read.
   * @return A reader for the specified number of bytes.
   */
  final def readByteArray(count: Int): BinaryReader[Array[Byte]] =
    liftF[Read, Array[Byte]](_.readBytes(count) map { buffer =>
      val result = new Array[Byte](count)
      ByteBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of bytes as a buffer from a binary input.
   *
   * @param count The number of bytes to read.
   * @return A reader for the specified number of bytes as a buffer.
   */
  final def readByteBuffer(count: Int): BinaryReader[ByteBuffer] =
    liftF[Read, ByteBuffer](_.readBytes(count) map { buffer =>
      val result = ByteBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads a single short from a binary input.
   *
   * @return A reader for a single short.
   */
  final def readShort(): BinaryReader[Short] =
    liftF[Read, Short](_.readShorts(1) map (_.get()))

  /**
   * Reads the specified number of shorts from a binary input.
   *
   * @param count The number of shorts to read.
   * @return A reader for the specified number of shorts.
   */
  final def readShortArray(count: Int): BinaryReader[Array[Short]] =
    liftF[Read, Array[Short]](_.readShorts(count) map { buffer =>
      val result = new Array[Short](count)
      ShortBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of shorts as a buffer from a binary input.
   *
   * @param count The number of shorts to read.
   * @return A reader for the specified number of shorts as a buffer.
   */
  final def readShortBuffer(count: Int): BinaryReader[ShortBuffer] =
    liftF[Read, ShortBuffer](_.readShorts(count) map { buffer =>
      val result = ShortBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads a single char from a binary input.
   *
   * @return A reader for a single char.
   */
  final def readChar(): BinaryReader[Char] =
    liftF[Read, Char](_.readChars(1) map (_.get()))

  /**
   * Reads the specified number of chars from a binary input.
   *
   * @param count The number of chars to read.
   * @return A reader for the specified number of chars.
   */
  final def readCharArray(count: Int): BinaryReader[Array[Char]] =
    liftF[Read, Array[Char]](_.readChars(count) map { buffer =>
      val result = new Array[Char](count)
      CharBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of chars as a buffer from a binary input.
   *
   * @param count The number of chars to read.
   * @return A reader for the specified number of chars as a buffer.
   */
  final def readCharBuffer(count: Int): BinaryReader[CharBuffer] =
    liftF[Read, CharBuffer](_.readChars(count) map { buffer =>
      val result = CharBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads a single int from a binary input.
   *
   * @return A reader for a single int.
   */
  final def readInt(): BinaryReader[Int] =
    liftF[Read, Int](_.readInts(1) map (_.get()))

  /**
   * Reads the specified number of ints from a binary input.
   *
   * @param count The number of ints to read.
   * @return A reader for the specified number of ints.
   */
  final def readIntArray(count: Int): BinaryReader[Array[Int]] =
    liftF[Read, Array[Int]](_.readInts(count) map { buffer =>
      val result = new Array[Int](count)
      IntBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of ints as a buffer from a binary input.
   *
   * @param count The number of ints to read.
   * @return A reader for the specified number of ints as a buffer.
   */
  final def readIntBuffer(count: Int): BinaryReader[IntBuffer] =
    liftF[Read, IntBuffer](_.readInts(count) map { buffer =>
      val result = IntBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads a single float from a binary input.
   *
   * @return A reader for a single float.
   */
  final def readFloat(): BinaryReader[Float] =
    liftF[Read, Float](_.readFloats(1) map (_.get()))

  /**
   * Reads the specified number of floats from a binary input.
   *
   * @param count The number of floats to read.
   * @return A reader for the specified number of floats.
   */
  final def readFloatArray(count: Int): BinaryReader[Array[Float]] =
    liftF[Read, Array[Float]](_.readFloats(count) map { buffer =>
      val result = new Array[Float](count)
      FloatBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of floats as a buffer from a binary input.
   *
   * @param count The number of floats to read.
   * @return A reader for the specified number of floats as a buffer.
   */
  final def readFloatBuffer(count: Int): BinaryReader[FloatBuffer] =
    liftF[Read, FloatBuffer](_.readFloats(count) map { buffer =>
      val result = FloatBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads a single long from a binary input.
   *
   * @return A reader for a single long.
   */
  final def readLong(): BinaryReader[Long] =
    liftF[Read, Long](_.readLongs(1) map (_.get()))

  /**
   * Reads the specified number of longs from a binary input.
   *
   * @param count The number of longs to read.
   * @return A reader for the specified number of longs.
   */
  final def readLongArray(count: Int): BinaryReader[Array[Long]] =
    liftF[Read, Array[Long]](_.readLongs(count) map { buffer =>
      val result = new Array[Long](count)
      LongBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of longs as a buffer from a binary input.
   *
   * @param count The number of longs to read.
   * @return A reader for the specified number of longs as a buffer.
   */
  final def readLongBuffer(count: Int): BinaryReader[LongBuffer] =
    liftF[Read, LongBuffer](_.readLongs(count) map { buffer =>
      val result = LongBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads a single double from a binary input.
   *
   * @return A reader for a single double.
   */
  final def readDouble(): BinaryReader[Double] =
    liftF[Read, Double](_.readDoubles(1) map (_.get()))

  /**
   * Reads the specified number of doubles from a binary input.
   *
   * @param count The number of doubles to read.
   * @return A reader for the specified number of doubles.
   */
  final def readDoubleArray(count: Int): BinaryReader[Array[Double]] =
    liftF[Read, Array[Double]](_.readDoubles(count) map { buffer =>
      val result = new Array[Double](count)
      DoubleBuffer.wrap(result).put(buffer)
      result
    })

  /**
   * Reads the specified number of doubles as a buffer from a binary input.
   *
   * @param count The number of doubles to read.
   * @return A reader for the specified number of doubles as a buffer.
   */
  final def readDoubleBuffer(count: Int): BinaryReader[DoubleBuffer] =
    liftF[Read, DoubleBuffer](_.readDoubles(count) map { buffer =>
      val result = DoubleBuffer.allocate(count)
      result.put(buffer)
      result.rewind()
      result
    })

  /**
   * Reads and encoded string as a character sequence from a binary input.
   *
   * @param charset The name of the character encoding to use when decoding the string.
   * @return A reader for a string as a character sequence.
   */
  final def readString(charset: String): BinaryReader[CharSequence] =
    readString(Charset.forName(charset))


  /**
   * Reads and encoded string as a character sequence from a binary input.
   *
   * @param charset The character encoding to use when decoding the string, defaults to UTF-8.
   * @return A reader for a string as a character sequence.
   */
  final def readString(charset: Charset = StandardCharsets.UTF_8): BinaryReader[CharSequence] =
    liftF[Read, CharSequence] { input =>
      for {
        count <- input.readInts(1)
        buffer <- input.readBytes(count.get())
        result <- Either.catchOnly[CharacterCodingException](charset.newDecoder().decode(buffer)).
          left.map(IOProblem.Decoding)
      } yield result
    }

  //
  // Pure binary binaryOutput operations.
  //

  /**
   * Writes a single byte to a binary binaryOutput.
   *
   * @param byte The byte to write.
   * @return A writer for a single byte.
   */
  final def writeByte(byte: Byte): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeBytes(1)(_.put(byte)))

  /**
   * Writes an array of bytes to a binary binaryOutput.
   *
   * @param array The array of bytes to write.
   * @return A writer for an array of bytes.
   */
  @inline
  final def writeByteArray(array: Array[Byte]): BinaryWriter[Unit] =
  writeByteArray(array, 0)

  /**
   * Writes an array of bytes to a binary binaryOutput.
   *
   * @param array  The array of bytes to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of bytes.
   */
  @inline
  final def writeByteArray(array: Array[Byte], offset: Int): BinaryWriter[Unit] =
  writeByteArray(array, offset, array.length - offset)

  /**
   * Writes an array of bytes to a binary binaryOutput.
   *
   * @param array  The array of bytes to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of bytes to write.
   * @return A writer for an array of bytes.
   */
  final def writeByteArray(array: Array[Byte], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeBytes(count)(_.put(array, offset, count)))

  /**
   * Writes a byte buffer to a binary binaryOutput.
   *
   * @param buffer The byte buffer to write.
   * @return A writer for a byte buffer.
   */
  final def writeByteBuffer(buffer: ByteBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeBytes(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a single short to a binary binaryOutput.
   *
   * @param short The short to write.
   * @return A writer for a single short.
   */
  final def writeShort(short: Short): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeShorts(1)(_.put(short)))

  /**
   * Writes an array of shorts to a binary binaryOutput.
   *
   * @param array The array of shorts to write.
   * @return A writer for an array of shorts.
   */
  @inline
  final def writeShortArray(array: Array[Short]): BinaryWriter[Unit] =
  writeShortArray(array, 0)

  /**
   * Writes an array of shorts to a binary binaryOutput.
   *
   * @param array  The array of shorts to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of shorts.
   */
  @inline
  final def writeShortArray(array: Array[Short], offset: Int): BinaryWriter[Unit] =
  writeShortArray(array, offset, array.length - offset)

  /**
   * Writes an array of shorts to a binary binaryOutput.
   *
   * @param array  The array of shorts to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of shorts to write.
   * @return A writer for an array of shorts.
   */
  final def writeShortArray(array: Array[Short], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeShorts(count)(_.put(array, offset, count)))

  /**
   * Writes a short buffer to a binary binaryOutput.
   *
   * @param buffer The short buffer to write.
   * @return A writer for a short buffer.
   */
  final def writeShortBuffer(buffer: ShortBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeShorts(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a single char to a binary binaryOutput.
   *
   * @param char The char to write.
   * @return A writer for a single char.
   */
  final def writeChar(char: Char): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeChars(1)(_.put(char)))

  /**
   * Writes an array of chars to a binary binaryOutput.
   *
   * @param array The array of chars to write.
   * @return A writer for an array of chars.
   */
  @inline
  final def writeCharArray(array: Array[Char]): BinaryWriter[Unit] =
  writeCharArray(array, 0)

  /**
   * Writes an array of chars to a binary binaryOutput.
   *
   * @param array  The array of chars to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of chars.
   */
  @inline
  final def writeCharArray(array: Array[Char], offset: Int): BinaryWriter[Unit] =
  writeCharArray(array, offset, array.length - offset)

  /**
   * Writes an array of chars to a binary binaryOutput.
   *
   * @param array  The array of chars to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of chars to write.
   * @return A writer for an array of chars.
   */
  final def writeCharArray(array: Array[Char], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeChars(count)(_.put(array, offset, count)))

  /**
   * Writes a char buffer to a binary binaryOutput.
   *
   * @param buffer The char buffer to write.
   * @return A writer for a char buffer.
   */
  final def writeCharBuffer(buffer: CharBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeChars(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a single int to a binary binaryOutput.
   *
   * @param int The int to write.
   * @return A writer for a single int.
   */
  final def writeInt(int: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeInts(1)(_.put(int)))

  /**
   * Writes an array of ints to a binary binaryOutput.
   *
   * @param array The array of ints to write.
   * @return A writer for an array of ints.
   */
  @inline
  final def writeIntArray(array: Array[Int]): BinaryWriter[Unit] =
  writeIntArray(array, 0)

  /**
   * Writes an array of ints to a binary binaryOutput.
   *
   * @param array  The array of ints to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of ints.
   */
  @inline
  final def writeIntArray(array: Array[Int], offset: Int): BinaryWriter[Unit] =
  writeIntArray(array, offset, array.length - offset)

  /**
   * Writes an array of ints to a binary binaryOutput.
   *
   * @param array  The array of ints to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of ints to write.
   * @return A writer for an array of ints.
   */
  final def writeIntArray(array: Array[Int], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeInts(count)(_.put(array, offset, count)))

  /**
   * Writes a int buffer to a binary binaryOutput.
   *
   * @param buffer The int buffer to write.
   * @return A writer for a int buffer.
   */
  final def writeIntBuffer(buffer: IntBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeInts(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a single float to a binary binaryOutput.
   *
   * @param float The float to write.
   * @return A writer for a single float.
   */
  final def writeFloat(float: Float): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeFloats(1)(_.put(float)))

  /**
   * Writes an array of floats to a binary binaryOutput.
   *
   * @param array The array of floats to write.
   * @return A writer for an array of floats.
   */
  @inline
  final def writeFloatArray(array: Array[Float]): BinaryWriter[Unit] =
  writeFloatArray(array, 0)

  /**
   * Writes an array of floats to a binary binaryOutput.
   *
   * @param array  The array of floats to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of floats.
   */
  @inline
  final def writeFloatArray(array: Array[Float], offset: Int): BinaryWriter[Unit] =
  writeFloatArray(array, offset, array.length - offset)

  /**
   * Writes an array of floats to a binary binaryOutput.
   *
   * @param array  The array of floats to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of floats to write.
   * @return A writer for an array of floats.
   */
  final def writeFloatArray(array: Array[Float], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeFloats(count)(_.put(array, offset, count)))

  /**
   * Writes a float buffer to a binary binaryOutput.
   *
   * @param buffer The float buffer to write.
   * @return A writer for a float buffer.
   */
  final def writeFloatBuffer(buffer: FloatBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeFloats(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a single long to a binary binaryOutput.
   *
   * @param long The long to write.
   * @return A writer for a single long.
   */
  final def writeLong(long: Long): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeLongs(1)(_.put(long)))

  /**
   * Writes an array of longs to a binary binaryOutput.
   *
   * @param array The array of longs to write.
   * @return A writer for an array of longs.
   */
  @inline
  final def writeLongArray(array: Array[Long]): BinaryWriter[Unit] =
  writeLongArray(array, 0)

  /**
   * Writes an array of longs to a binary binaryOutput.
   *
   * @param array  The array of longs to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of longs.
   */
  @inline
  final def writeLongArray(array: Array[Long], offset: Int): BinaryWriter[Unit] =
  writeLongArray(array, offset, array.length - offset)

  /**
   * Writes an array of longs to a binary binaryOutput.
   *
   * @param array  The array of longs to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of longs to write.
   * @return A writer for an array of longs.
   */
  final def writeLongArray(array: Array[Long], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeLongs(count)(_.put(array, offset, count)))

  /**
   * Writes a long buffer to a binary binaryOutput.
   *
   * @param buffer The long buffer to write.
   * @return A writer for a long buffer.
   */
  final def writeLongBuffer(buffer: LongBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeLongs(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a single double to a binary binaryOutput.
   *
   * @param double The double to write.
   * @return A writer for a single double.
   */
  final def writeDouble(double: Double): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeDoubles(1)(_.put(double)))

  /**
   * Writes an array of doubles to a binary binaryOutput.
   *
   * @param array The array of doubles to write.
   * @return A writer for an array of doubles.
   */
  @inline
  final def writeDoubleArray(array: Array[Double]): BinaryWriter[Unit] =
  writeDoubleArray(array, 0)

  /**
   * Writes an array of doubles to a binary binaryOutput.
   *
   * @param array  The array of doubles to write.
   * @param offset The offset into the array to start writing from.
   * @return A writer for an array of doubles.
   */
  @inline
  final def writeDoubleArray(array: Array[Double], offset: Int): BinaryWriter[Unit] =
  writeDoubleArray(array, offset, array.length - offset)

  /**
   * Writes an array of doubles to a binary binaryOutput.
   *
   * @param array  The array of doubles to write.
   * @param offset The offset into the array to start writing from.
   * @param count  The number of doubles to write.
   * @return A writer for an array of doubles.
   */
  final def writeDoubleArray(array: Array[Double], offset: Int, count: Int): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeDoubles(count)(_.put(array, offset, count)))

  /**
   * Writes a double buffer to a binary binaryOutput.
   *
   * @param buffer The double buffer to write.
   * @return A writer for a double buffer.
   */
  final def writeDoubleBuffer(buffer: DoubleBuffer): BinaryWriter[Unit] =
    liftF[Write, Unit](_.writeDoubles(buffer.remaining)(_.put(buffer.duplicate())))

  /**
   * Writes a string to a binary binaryOutput.
   *
   * @param string  The string to write.
   * @param charset The name of the character encoding to use when encoding the string.
   * @return A writer for a string.
   */
  final def writeString(string: CharSequence, charset: String): BinaryWriter[Unit] =
    writeString(string, Charset.forName(charset))

  /**
   * Writes a string to a binary binaryOutput.
   *
   * @param string  The string to write.
   * @param charset The character encoding to use when encoding the string, defaults to UTF-8.
   * @return A writer for a string.
   */
  final def writeString(string: CharSequence, charset: Charset = StandardCharsets.UTF_8): BinaryWriter[Unit] =
    liftF[Write, Unit] { output =>
      for {
        bytes <- Either.catchOnly[CharacterCodingException](charset.newEncoder().encode(CharBuffer.wrap(string))).
          left.map(IOProblem.Encoding)
        _ <- output.writeInts(1)(_.put(bytes.remaining))
        _ <- output.writeBytes(bytes.remaining)(_.put(bytes))
      } yield ()
    }

  //
  // Impure binary input operations.
  //

  /**
   * Creates a reader that reads into an array of bytes.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes The array to read into.
   * @return A reader that reads into an array of bytes and returns the number of bytes read.
   */
  def readBinaryData(bytes: Array[Byte]): BinaryReader[Int] =
    readBinaryData(bytes, 0)

  /**
   * Creates a reader that reads into an array of bytes.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes  The array to read into.
   * @param offset The offset into the array to start reading into.
   * @return A reader that reads into an array of bytes and returns the number of bytes read.
   */
  def readBinaryData(bytes: Array[Byte], offset: Int): BinaryReader[Int] =
    readBinaryData(bytes, offset, bytes.length - offset)

  /**
   * Creates a reader that reads into an array of bytes.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes  The array to read into.
   * @param offset The offset into the array to start reading into.
   * @param count  The maximum number of bytes to read.
   * @return A reader that reads into an array of bytes and returns the number of bytes read.
   */
  def readBinaryData(bytes: Array[Byte], offset: Int, count: Int): BinaryReader[Int] =
    readBinaryData(ByteBuffer.wrap(bytes, offset, count))

  /**
   * Creates a reader that reads into a byte buffer.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes The buffer to read into.
   * @return A reader that reads into a byte buffer and returns the number of bytes read.
   */
  def readBinaryData(bytes: ByteBuffer): BinaryReader[Int] =
    liftF[Read, Int](_ (bytes))

  //
  // Impure binary binaryOutput operations.
  //

  /**
   * Creates a writer that writes from an array of bytes.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes The array to write from.
   * @return A writer that writes from an array of bytes and returns the number of bytes written.
   */
  def writeBinaryData(bytes: Array[Byte]): BinaryWriter[Int] =
    writeBinaryData(bytes, 0)

  /**
   * Creates a writer that writes from an array of bytes.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes  The array to write from.
   * @param offset The offset into the array to start writing from.
   * @return A writer that writes from an array of bytes and returns the number of bytes written.
   */
  def writeBinaryData(bytes: Array[Byte], offset: Int): BinaryWriter[Int] =
    writeBinaryData(bytes, offset, bytes.length - offset)

  /**
   * Creates a writer that writes from an array of bytes.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes  The array to write from.
   * @param offset The offset into the array to start writing from.
   * @param count  The maximum number of bytes to write.
   * @return A writer that writes from an array of bytes and returns the number of bytes written.
   */
  def writeBinaryData(bytes: Array[Byte], offset: Int, count: Int): BinaryWriter[Int] =
    writeBinaryData(ByteBuffer.wrap(bytes, offset, count))

  /**
   * Creates a writer that writes from a byte buffer.
   *
   * WARNING: This method is impure and care must be taken when composing this method with other IO operations.
   *
   * @param bytes The buffer to write from.
   * @return A writer that writes from a byte buffer and returns the number of bytes written.
   */
  def writeBinaryData(bytes: ByteBuffer): BinaryWriter[Int] =
    liftF[Write, Int](_ (bytes))

}

/**
 * Implementations of the supported binary IO operations and related types.
 */
object BinaryIO extends BinaryIO {

  import net.wayfarerx.dreamsleeve.io

  //
  // Publish binary inputs & binaryOutputs as well as IO problems & results.
  //

  /** Alias to the binary input type. */
  type BinaryInput = io.BinaryInput

  /** Alias to the binary input companion. */
  final val BinaryInput = io.BinaryInput

  /** Alias to the binary binaryOutput type. */
  type BinaryOutput = io.BinaryOutput

  /** Alias to the binary binaryOutput companion. */
  final val BinaryOutput = io.BinaryOutput

  /** Alias to the IO problem type. */
  type IOProblem = io.IOProblem

  /** Alias to the IO problem companion. */
  final val IOProblem = io.IOProblem

  /** Alias to the IO result type. */
  type IOResult[P <: IOProblem, T] = io.IOResult[P, T]

  /** Alias to the IO result companion. */
  final val IOResult = io.IOResult

  //
  // Define the reader & writer types, as well as their extensions.
  //

  /** The type of operations that read binary data. */
  type BinaryReader[T] = Free[Read, T]

  /** The type of operations that write binary data. */
  type BinaryWriter[T] = Free[Write, T]

  /**
   * Extensions to the binary reader type that support transforming an input into a result.
   *
   * @param reader The reader to extend.
   * @tparam T The type of the result of the reader.
   */
  final class BinaryReaderExtensions[T](val reader: BinaryReader[T]) extends AnyVal {

    /**
     * Transforms a binary input into an input result.
     *
     * @param input The input to transform.
     * @return The result of the transformed input.
     */
    def apply(input: BinaryInput): IOResult.Input[T] =
      reader.foldMap(BinaryIO.Read(input))

  }

  /**
   * Extensions to the binary writer type that support transforming an binaryOutput into a result.
   *
   * @param writer The writer to extend.
   * @tparam T The type of the result of the writer.
   */
  final class BinaryWriterExtensions[T](val writer: BinaryWriter[T]) extends AnyVal {

    /**
     * Transforms a binary binaryOutput into an binaryOutput result.
     *
     * @param binaryOutput The binaryOutput to transform.
     * @return The result of the transformed binaryOutput.
     */
    def apply(binaryOutput: BinaryOutput): IOResult.Output[T] =
      writer.foldMap(BinaryIO.Write(binaryOutput))

  }

  //
  // Define the binary IO operations.
  //

  /**
   * Base class for tasks that perform a binary read operation.
   *
   * @tparam T The type returned by this task.
   */
  trait Read[T] {

    /**
     * Applies this task to the specified contextual input object.
     *
     * @param input The contextual input object to read from.
     * @return The result of this read task.
     */
    def apply(input: BinaryInput): IOResult.Input[T]

  }

  /**
   * Implementations of the supported binary read operations.
   */
  object Read {

    /**
     * Creates an interpreter for read operations executed against the specified input object.
     *
     * @param input The input object to read from.
     * @return An interpreter for read operations executed against the specified input object.
     */
    def apply(input: BinaryInput): Read ~> IOResult.Input = new (Read ~> IOResult.Input) {
      override def apply[U](op: Read[U]): IOResult.Input[U] = op(input)
    }

  }

  /**
   * Base class for tasks that perform a binary write operation.
   *
   * @tparam T The type returned by this task.
   */
  trait Write[T] {

    /**
     * Applies this task to the specified contextual binaryOutput object.
     *
     * @param binaryOutput The contextual binaryOutput object to write to.
     * @return The result of this write task.
     */
    def apply(binaryOutput: BinaryOutput): IOResult.Output[T]

  }

  /**
   * Implementations of the supported binary write operations.
   */
  object Write {

    /**
     * Creates an interpreter for write operations executed against the specified binaryOutput object.
     *
     * @param binaryOutput The binaryOutput object to write to.
     * @return An interpreter for write operations executed against the specified binaryOutput object.
     */
    def apply(binaryOutput: BinaryOutput): Write ~> IOResult.Output = new (Write ~> IOResult.Output) {
      override def apply[U](op: Write[U]): IOResult.Output[U] = op(binaryOutput)
    }

  }

}
