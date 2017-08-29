/*
 * HasherSpec.scala
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
 * Test case for the hasher implementation.
 */
class HasherSpec extends FlatSpec with Matchers {

  private val digest = MessageDigest.getInstance("SHA-256")

  private val booleans = {
    digest.update(0xFF.toByte)
    Hash(digest.digest())
  }
  private val bytes = {
    digest.update(42.toByte)
    Hash(digest.digest())
  }
  private val shorts = {
    for (i <- 0 to 1) digest.update((42.toShort >>> (1 - i) * 8 & 0x000000FF).toByte)
    Hash(digest.digest())
  }
  private val chars = {
    for (i <- 0 to 1) digest.update(('x' >>> (1 - i) * 8 & 0x000000FF).toByte)
    Hash(digest.digest())
  }
  private val ints = {
    for (i <- 0 to 3) digest.update((42 >>> (3 - i) * 8 & 0x000000FF).toByte)
    Hash(digest.digest())
  }
  private val floats = {
    val value = java.lang.Float.floatToIntBits(Math.PI.toFloat)
    for (i <- 0 to 3) digest.update((value >>> (3 - i) * 8 & 0x000000FF).toByte)
    Hash(digest.digest())
  }
  private val longs = {
    for (i <- 0 to 7) digest.update((42L >>> (7 - i) * 8 & 0x00000000000000FF).toByte)
    Hash(digest.digest())
  }
  private val doubles = {
    val value = java.lang.Double.doubleToRawLongBits(Math.PI)
    for (i <- 0 to 7) digest.update((value >>> (7 - i) * 8 & 0x00000000000000FF).toByte)
    Hash(digest.digest())
  }
  private val strings = {
    digest.update("hello".getBytes("UTF-8"))
    Hash(digest.digest())
  }
  private val hashes = {
    digest.update(strings.toBytes)
    Hash(digest.digest())
  }
  private val collections = {
    for (c <- "hello") for (i <- 0 to 1) digest.update((c >>> (1 - i) * 8 & 0x000000FF).toByte)
    Hash(digest.digest())
  }

  private val twoComponents = {
    digest.update(0.toByte)
    digest.update(42.toByte)
    Hash(digest.digest())
  }

  private val threeComponents = {
    digest.update(0.toByte)
    digest.update(42.toByte)
    digest.update("hello".getBytes("UTF-8"))
    Hash(digest.digest())
  }

  private val fourComponents = {
    digest.update(0.toByte)
    digest.update(42.toByte)
    digest.update("hello".getBytes("UTF-8"))
    digest.update(strings.toBytes)
    Hash(digest.digest())
  }

  "A hasher" should "consistently hash individual components" in {
    val hasher = Hasher()
    hasher(true) shouldBe booleans
    hasher(42.toByte) shouldBe bytes
    hasher(42.toShort) shouldBe shorts
    hasher('x') shouldBe chars
    hasher(42) shouldBe ints
    hasher(Math.PI.toFloat) shouldBe floats
    hasher(42L) shouldBe longs
    hasher(Math.PI) shouldBe doubles
    hasher("hello") shouldBe strings
    hasher(strings) shouldBe hashes
    hasher("hello".toVector) shouldBe collections
  }

  "A hasher" should "consistently hash heterogeneous components" in {
    val hasher = implicitly[Hasher]
    hasher(false, 42.toByte) shouldBe twoComponents
    hasher(false, 42.toByte, "hello") shouldBe threeComponents
    hasher(false, 42.toByte, "hello", strings) shouldBe fourComponents
  }

}
