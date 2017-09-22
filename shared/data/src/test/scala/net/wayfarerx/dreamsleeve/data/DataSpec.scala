/*
 * DataSpec.scala
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

import cats.Eval

import org.scalatest._

/**
 * Test case for the hashable support implementation.
 */
class DataSpec extends FlatSpec with Matchers {

  "A data element" should "consistently implement equals, toString and hashCode" in {
    TestData.equals(TestData: Any) shouldBe true
    TestData.toString shouldBe "TestData"
    TestData.hash eq TestData.hash shouldBe true
    TestData.hashCode() shouldBe
      (Hash.getInternalRepresentation(TestData.hash)(0) & 0x000000FF) << 24 |
        (Hash.getInternalRepresentation(TestData.hash)(1) & 0x000000FF) << 16 |
        (Hash.getInternalRepresentation(TestData.hash)(2) & 0x000000FF) << 8 |
        Hash.getInternalRepresentation(TestData.hash)(3) & 0x000000FF
  }

  /**
   * The data to test.
   */
  object TestData extends Data {

    val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

    /* Test for equality with this test. */
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case _: TestData.type => Eval.now(true)
      case _ => Eval.now(false)
    }

    /* Calculate the string for this test. */
    override protected[data] def calculateToString(): Eval[String] =
      Eval.now("TestData")

    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] = {
      generator.hash(false)
      val boolean = generator.hash(true)
      val byte = generator.hash(0x7D.toByte)
      val long = generator.hash(0x7D6C5B4A39281706L)
      val double = generator.hash(Math.PI)
      val string = generator.hash("hello")
      val two = generator.hash(boolean, byte)
      val three = generator.hash(boolean, byte, long)
      val four = generator.hash(boolean, byte, long, double)
      val collection = generator.hash(Seq(boolean, byte, long, double, string, two, three, four))
      val result = generator.hash(collection)
      boolean shouldBe hashItem(true)
      byte shouldBe hashItem(0x7D.toByte)
      long shouldBe hashItem(0x7D6C5B4A39281706L)
      double shouldBe hashItem(Math.PI)
      string shouldBe hashItem("hello")
      two shouldBe hashItem(Seq(boolean, byte))
      three shouldBe hashItem(Seq(boolean, byte, long))
      four shouldBe hashItem(Seq(boolean, byte, long, double))
      collection shouldBe hashItem(Seq(boolean, byte, long, double, string, two, three, four))
      result shouldBe generator.hash(collection)
      Eval.now(result)
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
