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

  //
  // Various expected document and fragment hashes.
  //
  private val boolean = {
    digest.update(Value.Boolean.Header)
    digest.update(0xFF.toByte)
    Hash(digest.digest())
  }
  private val number = {
    digest.update(Value.Number.Header)
    val value = java.lang.Double.doubleToRawLongBits(Math.PI)
    for (i <- 0 to 7) digest.update((value >>> (7 - i) * 8 & 0x00000000000000FF).toByte)
    Hash(digest.digest())
  }
  private val string = {
    digest.update(Value.String.Header)
    digest.update("hello".getBytes("UTF-8"))
    Hash(digest.digest())
  }
  private val table = {
    val inner = {
      digest.update(Table.Header)
      digest.update(boolean.toBytes)
      digest.update(boolean.toBytes)
      digest.update(number.toBytes)
      digest.update(number.toBytes)
      digest.update(string.toBytes)
      digest.update(string.toBytes)
      Hash(digest.digest())
    }
    digest.update(Table.Header)
    digest.update(string.toBytes)
    digest.update(inner.toBytes)
    Hash(digest.digest())
  }
  private val document = {
    digest.update(Document.Header)
    digest.update("doc".getBytes("UTF-8"))
    digest.update(table.toBytes)
    Hash(digest.digest())
  }

  //
  // Various expected difference and change hashes.
  //
  private val add = {
    digest.update(Change.Add.Header)
    digest.update(string.toBytes)
    Hash(digest.digest())
  }
  private val remove = {
    digest.update(Change.Remove.Header)
    digest.update(string.toBytes)
    Hash(digest.digest())
  }
  private val copy = {
    digest.update(Change.Copy.Header)
    digest.update(string.toBytes)
    Hash(digest.digest())
  }
  private val replace = {
    digest.update(Change.Replace.Header)
    digest.update(table.toBytes)
    digest.update(string.toBytes)
    Hash(digest.digest())
  }
  private val modify = {
    digest.update(Change.Modify.Header)
    digest.update(table.toBytes)
    digest.update(string.toBytes)
    digest.update(replace.toBytes)
    Hash(digest.digest())
  }
  private val create = {
    digest.update(Difference.Create.Header)
    digest.update(document.toBytes)
    Hash(digest.digest())
  }
  private val revise = {
    digest.update(Difference.Revise.Header)
    digest.update(document.toBytes)
    digest.update("doc".getBytes("UTF-8"))
    digest.update(modify.toBytes)
    Hash(digest.digest())
  }
  private val delete = {
    digest.update(Difference.Delete.Header)
    digest.update(document.toBytes)
    Hash(digest.digest())
  }

  "A hasher" should "consistently hash documents and fragments" in {
    val hasher = implicitly[Hasher]
    hasher.hashBoolean(true) shouldBe boolean
    hasher.hashNumber(Math.PI) shouldBe number
    hasher.hashString("hello") shouldBe string
    hasher.hashTable(
      Seq(string, hasher.hashTable(Seq(boolean, boolean, number, number, string, string)))
    ) shouldBe table
    hasher.hashDocument("doc", table) shouldBe document
  }

  it should "consistently hash differences and changes" in {
    val hasher = implicitly[Hasher]
    hasher.hashAdd(string) shouldBe add
    hasher.hashRemove(string) shouldBe remove
    hasher.hashCopy(string) shouldBe copy
    hasher.hashReplace(table, string) shouldBe replace
    hasher.hashModify(table, Seq(string, replace)) shouldBe modify
    hasher.hashCreate(document) shouldBe create
    hasher.hashRevise(document, "doc", modify) shouldBe revise
    hasher.hashDelete(document) shouldBe delete
  }

}
