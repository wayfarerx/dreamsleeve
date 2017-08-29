/*
 * HashSpec.scala
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

import org.scalatest._

/**
 * Test case for the hash implementation.
 */
class HashSpec extends FlatSpec with Matchers {

  /** The first hash to test with. */
  private val hash1 = Hash.setInternalRepresentation(Array[Byte](
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
    31, 32))
  /** The second hash to test with. */
  private val hash2 = Hash.setInternalRepresentation(hash1.toBytes)
  /** The third hash to test with. */
  private val hash3 = Hash.setInternalRepresentation(Hash.getInternalRepresentation(hash1).reverse)

  "A Hash" should "implement object identity rules like a value" in {
    hash1.equals(5: Any) shouldBe false
    hash2.equals(Vector(): Any) shouldBe false
    hash3.equals("hi": Any) shouldBe false
    hash1 == hash2 shouldBe true
    hash1 == hash3 shouldBe false
    hash3 == hash2 shouldBe false
    hash1.hashCode == hash2.hashCode shouldBe true
    hash1.hashCode == hash3.hashCode shouldBe false
    hash3.hashCode == hash2.hashCode shouldBe false
  }

  it should "support encoding and decoding from byte arrays" in {
    hash1.toBytes sameElements hash2.toBytes shouldBe true
    hash1.toBytes sameElements hash3.toBytes shouldBe false
    hash3.toBytes sameElements hash2.toBytes shouldBe false
    val bytes = hash3.toBytes
    bytes sameElements Hash.getInternalRepresentation(hash3) shouldBe true
    bytes eq Hash.getInternalRepresentation(hash3) shouldBe false
    val hash4 = Hash(bytes)
    bytes sameElements Hash.getInternalRepresentation(hash4) shouldBe true
    bytes eq Hash.getInternalRepresentation(hash4) shouldBe false
    hash3 == hash4 shouldBe true
    an[IllegalArgumentException] should be thrownBy Hash(Array[Byte]())
    an[IllegalArgumentException] should be thrownBy Hash(Array[Byte](7, 14, 21))
    an[IllegalArgumentException] should be thrownBy Hash((1 to 33).map(_.toByte).toArray)
  }

  it should "support encoding and decoding from strings" in {
    hash1.toString == hash2.toString shouldBe true
    hash1.toString == hash3.toString shouldBe false
    hash3.toString == hash2.toString shouldBe false
    val string = hash3.toString
    string shouldBe "201f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201"
    val hash4 = Hash(string)
    string == hash4.toString shouldBe true
    hash3 == hash4 shouldBe true
    val hash5 = Hash(string.toUpperCase)
    string == hash5.toString shouldBe true
    hash3 == hash5 shouldBe true
    an[IllegalArgumentException] should be thrownBy Hash("")
    an[IllegalArgumentException] should be thrownBy Hash("070e15".toCharArray)
    an[IllegalArgumentException] should be thrownBy Hash("21" + string)
    an[IllegalArgumentException] should be thrownBy Hash(string.replace('1', 'q'))
    an[IllegalArgumentException] should be thrownBy Hash(string.replace('f', 'q'))
  }

  it should "support encoding strings of the hash prefix" in {
    hash1.toPrefixString() shouldBe "0102030405"
    hash1.toPrefixString(0) shouldBe ""
    hash1.toPrefixString(10) shouldBe "0102030405060708090a"
    hash1.toPrefixString(32) shouldBe "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20"
    an[IllegalArgumentException] should be thrownBy hash1.toPrefixString(-1)
    an[IllegalArgumentException] should be thrownBy hash1.toPrefixString(33)
  }

}

