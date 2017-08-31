/*
 * DifferenceSpec.scala
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
 * Test case for the difference implementations.
 */
class DifferenceSpec extends FlatSpec with Matchers {

  import Difference._

  "A create" should "act as a hashable creation of a document" in {
    val d = Document("e", Table())
    Create(d).hash shouldBe Hasher()(Create.Header, d.hash)
    Create.unapply(Create(d)) shouldBe Some(d)
    Difference.unapply(Create(d)) shouldBe true
  }

  "A revise" should "verify the hash of a document and apply a change" in {
    val t1 = Table(Value.String("a") -> Value.Number(1))
    val t2 = Table(Value.String("a") -> Value.Number(2))
    val d1 = Document("e", t1)
    val d2 = Document("g", t2)
    val a = Revise(d1, d2.title, Update(t1, t2))
    a.hash shouldBe Hasher()(Revise.Header, d1.hash, d2.title, a.update.hash)
    Revise.unapply(a) shouldBe Some((d1.hash, d2.title, a.update))
    Difference.unapply(a) shouldBe true
  }

  "A delete" should "verify the hash of a document" in {
    val d = Document("e", Table())
    Delete(d).hash shouldBe Hasher()(Delete.Header, d.hash)
    Delete.unapply(Delete(d)) shouldBe Some(d.hash)
    Difference.unapply(Delete(d)) shouldBe true
  }

}
