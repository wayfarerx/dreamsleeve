/*
 * PatchingDifferencesSpec.scala
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

package net.wayfarerx.dreamsleeve.data.patching

import net.wayfarerx.dreamsleeve.data._
import org.scalatest._

/**
 * Test case for the difference patching implementations.
 */
class PatchingDifferencesSpec extends FlatSpec with Matchers {

  import Difference._

  "A create" should "patch the addition of a document" in {
    val d = Document("e", Table())
    Create(d).patch() shouldBe d
  }

  "A revise" should "patch a revision of a document" in {
    val d1 = Document("e", Table(Value.String("a") -> Value.Number(1)))
    val d2 = Document("f", Table(Value.String("a") -> Value.Number(1)))
    val d3 = Document("g", Table(Value.String("a") -> Value.Number(2)))
    val d4 = Document("h", Table(Value.String("a") -> Value.Number(3)))
    Revise(d1, d3).patch(d1) shouldBe Right(d3)
    Revise(d1, d2).patch(d2) shouldBe Left(Vector(Problems.HashMismatch(d1.hash, d2.hash)))
    Revise(d1, d3).patch(d4) shouldBe
      Left(Vector(Problems.HashMismatch(d1.hash, d4.hash),
        Problems.HashMismatch(d1.content.hash, d4.content.hash),
        Problems.HashMismatch(Value.Number(1).hash, Value.Number(3).hash)))
  }

  "A delete" should "patch the removal of a document" in {
    val d = Document("e", Table())
    Delete(d).patch(d) shouldBe true
    Delete(d).patch(Document("f", Table())) shouldBe false
  }

}
