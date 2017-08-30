/*
 * PatchingProblemSpec.scala
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
package patching_data

import org.scalatest._

import scala.collection.immutable.SortedSet

/**
 * Test case for the patching problem implementations.
 */
class PatchingProblemSpec extends FlatSpec with Matchers {

  import PatchingProblem._

  "Patching problems" should "construct and extract correctly" in {
    val v1: Value = Value.String("a")
    val v2: Value = Value.String("b")
    TypeMismatch.unapply(TypeMismatch(v1)) shouldBe Some(v1)
    HashMismatch.unapply(HashMismatch(v1.hash, v2.hash)) shouldBe Some((v1.hash, v2.hash))
    UnexpectedEntry.unapply(UnexpectedEntry(v1)) shouldBe Some(v1)
    MissingEntry.unapply(MissingEntry(v1)) shouldBe Some(v1)
    MismatchedEntries.unapply(MismatchedEntries(SortedSet(v1, v2))) shouldBe Some(SortedSet(v1, v2))
    PatchingProblem.toString
  }

}
