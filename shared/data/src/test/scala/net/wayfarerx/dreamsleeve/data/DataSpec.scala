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

import cats.implicits._

import org.scalatest._

/**
 * Test case for the hashable support implementation.
 */
class DataSpec extends FlatSpec with Matchers {

  "A data element" should "consistently implement equality and hashing" in {
    TestData.equals(TestData: Any) shouldBe true
    TestData.hashCode() shouldBe
      (Hash.getInternalRepresentation(TestData.hash)(0) & 0x000000FF) << 24 |
        (Hash.getInternalRepresentation(TestData.hash)(1) & 0x000000FF) << 16 |
        (Hash.getInternalRepresentation(TestData.hash)(2) & 0x000000FF) << 8 |
        Hash.getInternalRepresentation(TestData.hash)(3) & 0x000000FF
    Data.toString
  }

  /**
   * The data to test.
   */
  object TestData extends Data {

    /* Test for equality with this revise. */
    override protected def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      _ <- EqualsTask.ofType[TestData.type](that)
      v <- EqualsTask.areEqual(true, true)
    } yield v

    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(false)
    } yield h

  }

}
