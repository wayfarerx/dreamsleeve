/*
 * BinaryInputSpec.scala
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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.StandardCharsets

import org.scalatest._

/**
 * Test case for the binary input API.
 */
class BinaryInputSpec extends FlatSpec with Matchers {

  "The binary input API" should "support in-memory data sources" in {
    val buffer = ByteBuffer.allocate(4)
    val singleInput: BinaryInput = Array[Byte](7)
    singleInput.read(buffer) shouldBe Right(1)
    buffer.flip()
    buffer.remaining shouldBe 1
    buffer.get() shouldBe 7.toByte
    buffer.clear()
    val multipleInput: BinaryInput = Array[Byte](1, 3, 5, 7, 11)
    multipleInput.read(buffer) shouldBe Right(4)
    buffer.flip()
    buffer.remaining shouldBe 4
    buffer.get() shouldBe 1.toByte
    buffer.get() shouldBe 3.toByte
    buffer.get() shouldBe 5.toByte
    buffer.get() shouldBe 7.toByte
    buffer.clear()
    multipleInput.read(buffer) shouldBe Right(1)
    buffer.flip()
    buffer.remaining shouldBe 1
    buffer.get() shouldBe 11.toByte
    buffer.clear()
    val emptyInput: BinaryInput = ByteBuffer.allocate(0)
    emptyInput.read(buffer) shouldBe Left(IOProblem.Underflow)
    val noOpBuffer = ByteBuffer.allocate(1)
    val noOpInput: BinaryInput = noOpBuffer
    noOpInput.read(ByteBuffer.allocate(0)) shouldBe Right(0)
    noOpBuffer.remaining shouldBe 1
  }

  it should "support external data sources" in {
    val buffer = ByteBuffer.allocate(4)
    val streamInput: BinaryInput = new ByteArrayInputStream(Array[Byte](1, 3, 5, 7, 11, 13, 17, 19))
    streamInput.read(buffer) shouldBe Right(4)
    buffer.flip()
    buffer.get() shouldBe 1.toByte
    buffer.get() shouldBe 3.toByte
    buffer.get() shouldBe 5.toByte
    buffer.get() shouldBe 7.toByte
    buffer.clear()
    streamInput.read(buffer) shouldBe Right(4)
    buffer.flip()
    buffer.get() shouldBe 11.toByte
    buffer.get() shouldBe 13.toByte
    buffer.get() shouldBe 17.toByte
    buffer.get() shouldBe 19.toByte
    buffer.clear()
    streamInput.read(buffer) shouldBe Left(IOProblem.Underflow)
    val noOpStream = new ByteArrayInputStream(Array[Byte](7))
    val noOpInput: BinaryInput = noOpStream
    noOpInput.read(ByteBuffer.allocate(0)) shouldBe Right(0)
    noOpStream.read() shouldBe 7
    val e = new IOException
    val channelInput: BinaryInput = new ReadableByteChannel {
      override def read(dst: ByteBuffer): Int = throw e

      override def isOpen: Boolean = true

      override def close(): Unit = ()
    }
    channelInput.read(buffer) shouldBe Left(IOProblem.Failure(e))
  }

  it should "provide operations for fully reading primitives" in {
    val bytes = new ByteArrayOutputStream()
    val data = new DataOutputStream(bytes)
    data.writeByte(1)
    data.writeShort(3)
    data.writeChar('5')
    data.writeInt(7)
    data.writeFloat(11.0f)
    data.writeLong(13L)
    data.writeDouble(17.0)
    data.flush()
    val input: BinaryInput = bytes.toByteArray
    input.readBytes(1)(_.get()) shouldBe Right(1.toByte)
    input.readShorts(1)(_.get()) shouldBe Right(3.toShort)
    input.readChars(1)(_.get()) shouldBe Right('5')
    input.readInts(1)(_.get()) shouldBe Right(7)
    input.readFloats(1)(_.get()) shouldBe Right(11.0f)
    input.readLongs(1)(_.get()) shouldBe Right(13L)
    input.readDoubles(1)(_.get()) shouldBe Right(17.0)
    val empty = new ByteArrayInputStream(Array[Byte](7))
    val emptyInput: BinaryInput = empty
    emptyInput.readBytes(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    emptyInput.readShorts(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    emptyInput.readChars(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    emptyInput.readInts(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    emptyInput.readFloats(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    emptyInput.readLongs(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    emptyInput.readDoubles(0) { b => b.remaining shouldBe 0; () } shouldBe Right(())
    empty.read() shouldBe 7
    an[IllegalArgumentException] should be thrownBy emptyInput.readBytes(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyInput.readShorts(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyInput.readChars(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyInput.readInts(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyInput.readFloats(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyInput.readLongs(-1)(_ => ())
    an[IllegalArgumentException] should be thrownBy emptyInput.readDoubles(-1)(_ => ())
  }

  it should "provide operations for fully reading strings" in {
    val charset = StandardCharsets.UTF_8
    val bytes = new ByteArrayOutputStream
    val data = new DataOutputStream(bytes)
    data.writeUTF("Hello")
    data.flush()
    val input: BinaryInput = bytes.toByteArray
    input.readString(charset) map (_.toString) shouldBe Right("Hello")
    val empty = ByteBuffer.allocate(2)
    empty.asShortBuffer.put(0, 0)
    val emptyInput: BinaryInput = empty
    emptyInput.readString(charset) map (_.toString) shouldBe Right("")
    val invalidBuffer = ByteBuffer.allocate(4)
    invalidBuffer.asShortBuffer.put(2.toShort)
    invalidBuffer.asCharBuffer.put(1, '\uD800')
    rewindBuffer(invalidBuffer)
    val invalidInput: BinaryInput = invalidBuffer
    invalidInput.readString(charset) match {
      case Left(IOProblem.Decoding(_)) =>
      case other => fail(other.toString)
    }
  }

  it should "attempt to fill buffers and underflow if it cannot" in {
    var count = 2
    val input: BinaryInput = new ReadableByteChannel {
      override def read(dst: ByteBuffer): Int = {
        for (i <- 0 until count) dst.put(i.toByte)
        count
      }

      override def isOpen: Boolean = true

      override def close(): Unit = ()
    }
    input.readBytes(4)(_.remaining()) shouldBe Right(4)
    count = 0
    input.readBytes(1)(_.remaining()) shouldBe Left(IOProblem.Underflow)
  }

}
