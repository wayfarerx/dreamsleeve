/*
 * PatchingChangesSpec.scala
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

/**
 * Test case for the change patching implementations.
 */
class PatchingChangesSpec extends FlatSpec with Matchers {

  import Change._

  "An add" should "patch the addition of a fragment to a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Add(fa)
    val b = Add(fb)
    a.patch() shouldBe Right(fa)
    b.patch() shouldBe Right(fb)
  }

  "A remove" should "patch the removal of a fragment from a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Remove(fa)
    val b = Remove(fb.hash)
    a.patch(fa) shouldBe Right(())
    a.patch(fb) shouldBe Left(Problems.HashMismatch(fa.hash, fb.hash))
    b.patch(fa) shouldBe Left(Problems.HashMismatch(fb.hash, fa.hash))
    b.patch(fb) shouldBe Right(())
  }

}
