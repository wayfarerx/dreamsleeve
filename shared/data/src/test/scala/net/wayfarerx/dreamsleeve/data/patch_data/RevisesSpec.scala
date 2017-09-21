/*
 * RevisesSpec.scala
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
package patch_data

import org.scalatest._

/**
 * Test case for the revise patching implementation.
 */
class RevisesSpec extends FlatSpec with Matchers {

  "A revise" should "patch a revision of a document" in {
    val d1 = Document("e", Table(Value.String("a") -> Value.Number(1)))
    val d2 = Document("f", Table(Value.String("a") -> Value.Number(1)))
    val d3 = Document("g", Table(Value.String("a") -> Value.Number(2)))
    val d4 = Document("h", Table(Value.String("a") -> Value.Number(3)))
    Difference.Revise(d1, d3).patch(d1) shouldBe Right(d3)
    Difference.Revise(d1, d2).patch(d2) shouldBe Left(PatchProblem.HashMismatch(d1.hash, d2.hash))
    Difference.Revise(d1, d3).patch(d4) shouldBe Left(PatchProblem.HashMismatch(d1.hash, d4.hash))
  }

}
