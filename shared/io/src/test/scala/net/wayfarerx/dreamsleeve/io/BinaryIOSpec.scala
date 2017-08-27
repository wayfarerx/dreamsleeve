/*
 * BinaryIOSpec.scala
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

import java.io.ByteArrayOutputStream
import java.nio._

import org.scalatest._

/**
 * Test case for the binary IO API.
 */
class BinaryIOSpec extends FlatSpec with Matchers {

  "The binary IO API" should "support reading and writing bytes" in {
    val buffer = ByteBuffer.wrap(Array[Byte](7, 11))
    val writer = for {
      _ <- writeByte(1.toByte)
      _ <- writeByteArray(Array[Byte](3, 5))
      _ <- writeByteBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readByte()
      a <- readByteArray(2)
      b <- readByteBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right((1.toByte, 3.toByte, 5.toByte, 7.toByte, 11.toByte))
    val emptyWriter = for {
      _ <- writeByteArray(Array[Byte]())
      _ <- writeByteBuffer(ByteBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readByteArray(0)
      f <- readByteBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readByteArray(-1)
    an[IllegalArgumentException] should be thrownBy readByteBuffer(-1)
  }

  it should "support reading and writing shorts" in {
    val buffer = ShortBuffer.wrap(Array[Short](7, 11))
    val writer = for {
      _ <- writeShort(1.toShort)
      _ <- writeShortArray(Array[Short](3, 5))
      _ <- writeShortBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readShort()
      a <- readShortArray(2)
      b <- readShortBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right((1.toShort, 3.toShort, 5.toShort, 7.toShort, 11.toShort))
    val emptyWriter = for {
      _ <- writeShortArray(Array[Short]())
      _ <- writeShortBuffer(ShortBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readShortArray(0)
      f <- readShortBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readShortArray(-1)
    an[IllegalArgumentException] should be thrownBy readShortBuffer(-1)
  }

  it should "support reading and writing chars" in {
    val buffer = CharBuffer.wrap(Array[Char]('7', '0'))
    val writer = for {
      _ <- writeChar('1')
      _ <- writeCharArray(Array[Char]('3', '5'))
      _ <- writeCharBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readChar()
      a <- readCharArray(2)
      b <- readCharBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right(('1', '3', '5', '7', '0'))
    val emptyWriter = for {
      _ <- writeCharArray(Array[Char]())
      _ <- writeCharBuffer(CharBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readCharArray(0)
      f <- readCharBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readCharArray(-1)
    an[IllegalArgumentException] should be thrownBy readCharBuffer(-1)
  }

  it should "support reading and writing ints" in {
    val buffer = IntBuffer.wrap(Array[Int](7, 11))
    val writer = for {
      _ <- writeInt(1)
      _ <- writeIntArray(Array[Int](3, 5))
      _ <- writeIntBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readInt()
      a <- readIntArray(2)
      b <- readIntBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right((1, 3, 5, 7, 11))
    val emptyWriter = for {
      _ <- writeIntArray(Array[Int]())
      _ <- writeIntBuffer(IntBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readIntArray(0)
      f <- readIntBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readIntArray(-1)
    an[IllegalArgumentException] should be thrownBy readIntBuffer(-1)
  }

  it should "support reading and writing floats" in {
    val buffer = FloatBuffer.wrap(Array[Float](7f, 11f))
    val writer = for {
      _ <- writeFloat(1f)
      _ <- writeFloatArray(Array[Float](3f, 5f))
      _ <- writeFloatBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readFloat()
      a <- readFloatArray(2)
      b <- readFloatBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right((1f, 3f, 5f, 7f, 11f))
    val emptyWriter = for {
      _ <- writeFloatArray(Array[Float]())
      _ <- writeFloatBuffer(FloatBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readFloatArray(0)
      f <- readFloatBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readFloatArray(-1)
    an[IllegalArgumentException] should be thrownBy readFloatBuffer(-1)
  }

  it should "support reading and writing longs" in {
    val buffer = LongBuffer.wrap(Array[Long](7L, 11L))
    val writer = for {
      _ <- writeLong(1L)
      _ <- writeLongArray(Array[Long](3L, 5L))
      _ <- writeLongBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readLong()
      a <- readLongArray(2)
      b <- readLongBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right((1L, 3L, 5L, 7L, 11L))
    val emptyWriter = for {
      _ <- writeLongArray(Array[Long]())
      _ <- writeLongBuffer(LongBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readLongArray(0)
      f <- readLongBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readLongArray(-1)
    an[IllegalArgumentException] should be thrownBy readLongBuffer(-1)
  }

  it should "support reading and writing doubles" in {
    val buffer = DoubleBuffer.wrap(Array[Double](7.0, 11.0))
    val writer = for {
      _ <- writeDouble(1.0)
      _ <- writeDoubleArray(Array[Double](3.0, 5.0))
      _ <- writeDoubleBuffer(buffer)
    } yield ()
    val reader = for {
      v <- readDouble()
      a <- readDoubleArray(2)
      b <- readDoubleBuffer(2)
    } yield (v, a(0), a(1), b.get(), b.get())
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    buffer.remaining shouldBe 2
    reader(stream.toByteArray) shouldBe Right((1.0, 3.0, 5.0, 7.0, 11.0))
    val emptyWriter = for {
      _ <- writeDoubleArray(Array[Double]())
      _ <- writeDoubleBuffer(DoubleBuffer.allocate(0))
    } yield ()
    val emptyReader = for {
      e <- readDoubleArray(0)
      f <- readDoubleBuffer(0)
    } yield e.length + f.remaining
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    emptyReader(empty.toByteArray) shouldBe Right(0)
    an[IllegalArgumentException] should be thrownBy readDoubleArray(-1)
    an[IllegalArgumentException] should be thrownBy readDoubleBuffer(-1)
  }

  it should "support reading and writing strings" in {
    val writer = for {
      _ <- writeString("Hello")
      _ <- writeString("World", "ASCII")
    } yield ()
    val reader = for {
      h <- readString()
      w <- readString("ASCII")
    } yield (h.toString, w.toString)
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(())
    reader(stream.toByteArray) shouldBe Right(("Hello", "World"))
    val emptyWriter = for {_ <- writeString("")} yield ()
    val emptyReader = for {e <- readString()} yield e.toString
    val empty = new ByteArrayOutputStream
    emptyWriter(empty) shouldBe Right(())
    emptyReader(empty.toByteArray) shouldBe Right("")
  }

  it should "support reading and writing raw binary data" in {
    val in = new Array[Byte](4)
    val out = Array[Byte](1, 3, 5, 7)
    val reader = for (count <- readBinaryData(in)) yield count
    val writer = for (count <- writeBinaryData(out)) yield count
    val stream = new ByteArrayOutputStream
    writer(stream) shouldBe Right(4)
    reader(stream.toByteArray) shouldBe Right(4)
    in(0) shouldBe 1.toByte
    in(1) shouldBe 3.toByte
    in(2) shouldBe 5.toByte
    in(3) shouldBe 7.toByte
  }

  it should "provide a subset of the entire IO API" in {
    // Poking for coverage metrics.
    BinaryIO.toString
    BinaryIO.BinaryInput.toString
    BinaryIO.BinaryOutput.toString
    BinaryIO.IOProblem.toString
    BinaryIO.IOResult.toString
    IOProblem.Unsupported.toString
  }

}
