/*
 * ValueSpec.scala
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
 * Test case for value implementations.
 */
class ValueSpec extends FlatSpec with Matchers {

  "A boolean value" should "act as a hashable, comparable value" in {
    val f = Value.Boolean()
    val t = Value.Boolean(true)
    f.hash shouldBe TestHashing(Value.Boolean.Header, false)
    t.hash shouldBe TestHashing(Value.Boolean.Header, true)
    f.compareTo(f) shouldBe 0
    f.compareTo(t) should be < 0
    t.compareTo(f) should be > 0
    t.compareTo(t) shouldBe 0
    f.compareTo(Value.Number()) should be < 0
    f.compareTo(Value.String()) should be < 0
    t.compareTo(Value.Number()) should be < 0
    t.compareTo(Value.String()) should be < 0
  }

  "A number value" should "act as a hashable, comparable value" in {
    val n = Value.Number(-1.0)
    val z = Value.Number()
    val p = Value.Number(Math.PI)
    n.hash shouldBe TestHashing(Value.Number.Header, -1.0)
    z.hash shouldBe TestHashing(Value.Number.Header, 0.0)
    p.hash shouldBe TestHashing(Value.Number.Header, Math.PI)
    n.compareTo(n) shouldBe 0
    n.compareTo(z) should be < 0
    n.compareTo(p) should be < 0
    z.compareTo(n) should be > 0
    z.compareTo(z) shouldBe 0
    z.compareTo(p) should be < 0
    p.compareTo(n) should be > 0
    p.compareTo(z) should be > 0
    p.compareTo(p) shouldBe 0
    n.compareTo(Value.Boolean()) should be > 0
    n.compareTo(Value.String()) should be < 0
    z.compareTo(Value.Boolean()) should be > 0
    z.compareTo(Value.String()) should be < 0
    p.compareTo(Value.Boolean()) should be > 0
    p.compareTo(Value.String()) should be < 0

  }

  "A string value" should "act as a hashable, comparable value" in {
    val e = Value.String()
    val a = Value.String("a")
    val z = Value.String("z")
    e.hash shouldBe TestHashing(Value.String.Header, "")
    a.hash shouldBe TestHashing(Value.String.Header, "a")
    z.hash shouldBe TestHashing(Value.String.Header, "z")
    e.compareTo(e) shouldBe 0
    e.compareTo(a) should be < 0
    e.compareTo(z) should be < 0
    a.compareTo(e) should be > 0
    a.compareTo(a) shouldBe 0
    a.compareTo(z) should be < 0
    z.compareTo(e) should be > 0
    z.compareTo(a) should be > 0
    z.compareTo(z) shouldBe 0
    e.compareTo(Value.Boolean()) should be > 0
    e.compareTo(Value.Number()) should be > 0
    a.compareTo(Value.Boolean()) should be > 0
    a.compareTo(Value.Number()) should be > 0
    z.compareTo(Value.Boolean()) should be > 0
    z.compareTo(Value.Number()) should be > 0
  }

  "Value" should "extract any value implementation" in {
    Value.unapply(Value.Boolean()) shouldBe true
    Value.unapply(Value.Number()) shouldBe true
    Value.unapply(Value.String()) shouldBe true
    ("": Any) match {
      case Value() => fail()
      case _ =>
    }
  }

}
