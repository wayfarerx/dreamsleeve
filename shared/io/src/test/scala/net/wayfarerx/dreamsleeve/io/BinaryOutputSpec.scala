/*
 * BinaryOutputSpec.scala
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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel
import java.nio.charset.StandardCharsets

import org.scalatest._

/**
 * Test case for the binary output API.
 */
class BinaryOutputSpec extends FlatSpec with Matchers {

  "The binary output API" should "support in-memory data destinations" in {
    val buffer = ByteBuffer.wrap(Array[Byte](1, 3, 5, 7))
    val singleArray = new Array[Byte](1)
    val singleOutput: BinaryOutput = singleArray
    singleOutput.write(buffer) shouldBe Right(1)
    singleArray(0) shouldBe 1.toByte
    buffer.clear()
    val multipleArray = new Array[Byte](8)
    val multipleOutput: BinaryOutput = multipleArray
    multipleOutput.write(buffer) shouldBe Right(4)
    multipleArray(0) shouldBe 1.toByte
    multipleArray(1) shouldBe 3.toByte
    multipleArray(2) shouldBe 5.toByte
    multipleArray(3) shouldBe 7.toByte
    buffer.clear()
    multipleOutput.write(buffer) shouldBe Right(4)
    multipleArray(4) shouldBe 1.toByte
    multipleArray(5) shouldBe 3.toByte
    multipleArray(6) shouldBe 5.toByte
    multipleArray(7) shouldBe 7.toByte
    buffer.clear()
    val emptyOutput: BinaryOutput = ByteBuffer.allocate(0)
    emptyOutput.write(buffer) shouldBe Left(IOProblem.Overflow)
    val noOpBuffer = ByteBuffer.allocate(1)
    val noOpOutput: BinaryOutput = noOpBuffer
    noOpOutput.write(ByteBuffer.allocate(0)) shouldBe Right(0)
    noOpBuffer.remaining shouldBe 1
  }

  it should "support external data destinations" in {
    val buffer = ByteBuffer.wrap(Array[Byte](1, 3, 5, 7))
    val stream = new ByteArrayOutputStream
    val streamOutput: BinaryOutput = stream
    streamOutput.write(buffer) shouldBe Right(4)
    stream.toByteArray()(0) shouldBe 1.toByte
    stream.toByteArray()(1) shouldBe 3.toByte
    stream.toByteArray()(2) shouldBe 5.toByte
    stream.toByteArray()(3) shouldBe 7.toByte
    buffer.clear()
    streamOutput.write(buffer) shouldBe Right(4)
    stream.toByteArray()(4) shouldBe 1.toByte
    stream.toByteArray()(5) shouldBe 3.toByte
    stream.toByteArray()(6) shouldBe 5.toByte
    stream.toByteArray()(7) shouldBe 7.toByte
    val noOpStream = new ByteArrayOutputStream
    val noOpOutput: BinaryOutput = noOpStream
    noOpOutput.write(ByteBuffer.allocate(0)) shouldBe Right(0)
    noOpStream.toByteArray.length shouldBe 0
    val e = new IOException
    val channelOutput: BinaryOutput = new WritableByteChannel {
      override def write(src: ByteBuffer): Int = throw e

      override def isOpen: Boolean = true

      override def close(): Unit = ()
    }
    channelOutput.write(buffer) shouldBe Left(IOProblem.Failure(e))
  }

  it should "provide operations for fully writing primitives" in {
    val bytes = new ByteArrayOutputStream()
    val output: BinaryOutput = bytes
    output.writeBytes(1)(_.put(1.toByte)) shouldBe Right(())
    output.writeShorts(1)(_.put(3.toShort)) shouldBe Right(())
    output.writeChars(1)(_.put('5')) shouldBe Right(())
    output.writeInts(1)(_.put(7)) shouldBe Right(())
    output.writeFloats(1)(_.put(11.0f)) shouldBe Right(())
    output.writeLongs(1)(_.put(13L)) shouldBe Right(())
    output.writeDoubles(1)(_.put(17.0)) shouldBe Right(())
    val data = new DataInputStream(new ByteArrayInputStream(bytes.toByteArray))
    data.readByte() shouldBe 1.toByte
    data.readShort() shouldBe 3.toShort
    data.readChar() shouldBe '5'
    data.readInt() shouldBe 7
    data.readFloat() shouldBe 11.0f
    data.readLong() shouldBe 13L
    data.readDouble() shouldBe 17.0
    val empty = new ByteArrayOutputStream()
    val emptyOutput: BinaryOutput = empty
    emptyOutput.writeBytes(0)(_.remaining shouldBe 0) shouldBe Right(())
    emptyOutput.writeShorts(0)(_.remaining shouldBe 0) shouldBe Right(())
    emptyOutput.writeChars(0)(_.remaining shouldBe 0) shouldBe Right(())
    emptyOutput.writeInts(0)(_.remaining shouldBe 0) shouldBe Right(())
    emptyOutput.writeFloats(0)(_.remaining shouldBe 0) shouldBe Right(())
    emptyOutput.writeLongs(0)(_.remaining shouldBe 0) shouldBe Right(())
    emptyOutput.writeDoubles(0)(_.remaining shouldBe 0) shouldBe Right(())
    empty.toByteArray.length shouldBe 0
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeBytes(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeShorts(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeChars(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeInts(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeFloats(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeLongs(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyOutput.writeDoubles(-1)(_ => ())
  }

  it should "provide operations for fully writing strings" in {
    val charset = StandardCharsets.UTF_8
    val bytes = new ByteArrayOutputStream
    val output: BinaryOutput = bytes
    output.writeString("Hello", charset) shouldBe Right(())
    new DataInputStream(new ByteArrayInputStream(bytes.toByteArray)).readUTF() shouldBe "Hello"
    val empty = ByteBuffer.wrap(new Array[Byte](2))
    val emptyOutput: BinaryOutput = empty
    emptyOutput.writeString("", charset)
    empty.remaining shouldBe 0
    empty.rewind()
    empty.asShortBuffer.get(0) shouldBe 0.toShort
    new DataInputStream(new ByteArrayInputStream(empty.array())).readUTF() shouldBe ""
    val invalidOutput: BinaryOutput = new ByteArrayOutputStream
    invalidOutput.writeString("1" * 65536, charset) shouldBe Left(IOProblem.Unsupported(65536))
    invalidOutput.writeString("a\uD800", charset) match {
      case Left(IOProblem.Encoding(_)) =>
      case other => fail(other.toString)
    }
  }

  it should "attempt to empty buffers and overflow if it cannot" in {
    var count = 2
    val output: BinaryOutput = new WritableByteChannel {
      override def write(dst: ByteBuffer): Int = {
        for (i <- 0 until count) dst.get()
        count
      }

      override def isOpen: Boolean = true

      override def close(): Unit = ()
    }
    output.writeBytes(4)(_.put(Array[Byte](1, 3, 5, 7))) shouldBe Right(())
    count = 0
    output.writeBytes(1)(_.put(7.toByte)) shouldBe Left(IOProblem.Overflow)
  }

}
