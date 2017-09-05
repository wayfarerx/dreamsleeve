/*
 * HashableSpec.scala
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

import java.security.MessageDigest

import org.scalatest._

/**
 * Test case for the hashable support implementation.
 */
class HashableSpec extends FlatSpec with Matchers {

  "A hashable element" should "provide reliable hashing support" in {
    TestHashable.hash eq TestHashable.hash shouldBe true
    Hashable.toString
  }

  /**
   * The data to test.
   */
  object TestHashable extends Hashable {

    val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

    override protected def calculateHash(): HashOperation[Hash] = for {
      _ <- HashTask.hash(false)
      boolean <- HashTask.hash(true)
      byte <- HashTask.hash(0x7D.toByte)
      long <- HashTask.hash(0x7D6C5B4A39281706L)
      double <- HashTask.hash(Math.PI)
      string <- HashTask.hash("hello")
      two <- HashTask.hash(boolean, byte)
      three <- HashTask.hash(boolean, byte, long)
      four <- HashTask.hash(boolean, byte, long, double)
      collection <- HashTask.hash(Seq(boolean, byte, long, double, string, two, three, four))
      result <- HashTask.hash(collection)
      none <- HashTask.pure(Hash("0000000000000000000000000000000000000000000000000000000000000000"))
    } yield {
      boolean shouldBe hashItem(true)
      byte shouldBe hashItem(0x7D.toByte)
      long shouldBe hashItem(0x7D6C5B4A39281706L)
      double shouldBe hashItem(Math.PI)
      string shouldBe hashItem("hello")
      two shouldBe hashItem(Seq(boolean, byte))
      three shouldBe hashItem(Seq(boolean, byte, long))
      four shouldBe hashItem(Seq(boolean, byte, long, double))
      collection shouldBe hashItem(Seq(boolean, byte, long, double, string, two, three, four))
      result shouldBe hashItem(collection)
      none shouldBe Hash("0000000000000000000000000000000000000000000000000000000000000000")
      result
    }

    def hashItem(i: Boolean): Hash = {
      if (i) digest.update(0xFF.toByte) else digest.update(0x00.toByte)
      Hash.setInternalRepresentation(digest.digest())
    }

    def hashItem(i: Byte): Hash = {
      digest.update(i)
      Hash.setInternalRepresentation(digest.digest())
    }

    def hashItem(i: Long): Hash = {
      for (j <- 0 to 7) digest.update((i >>> (7 - j) * 8 & 0x00000000000000FF).toByte)
      Hash.setInternalRepresentation(digest.digest())
    }

    def hashItem(i: Double): Hash =
      hashItem(java.lang.Double.doubleToLongBits(i))

    def hashItem(i: String): Hash = {
      digest.update(i.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      Hash.setInternalRepresentation(digest.digest())
    }

    def hashItem(i: Hash): Hash = {
      digest.update(Hash.getInternalRepresentation(i))
      Hash.setInternalRepresentation(digest.digest())
    }

    def hashItem(i: Iterable[Hash]): Hash = {
      for (ii <- i) digest.update(Hash.getInternalRepresentation(ii))
      Hash.setInternalRepresentation(digest.digest())
    }

  }


}
