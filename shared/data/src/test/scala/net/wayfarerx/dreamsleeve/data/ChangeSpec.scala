/*
 * ChangeSpec.scala
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
 * Test case for the change implementations.
 */
class ChangeSpec extends FlatSpec with Matchers {

  import Change._

  "An add" should "act as a hashable addition of a fragment to a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    Add(fa).hash shouldBe Hasher()(Add.Header, fa.hash)
    Add(fb).hash shouldBe Hasher()(Add.Header, fb.hash)
    Add.unapply(Add(fa)) shouldBe Some(fa)
    Change.unapply(Add(fb)) shouldBe true
  }

  "A remove" should "act as a hashable removal of a fragment from a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    Remove(fa).hash shouldBe Hasher()(Remove.Header, fa.hash)
    Remove(fb.hash).hash shouldBe Hasher()(Remove.Header, fb.hash)
    Remove.unapply(Remove(fa)) shouldBe Some(fa.hash)
    Change.unapply(Remove(fb)) shouldBe true
  }

}
