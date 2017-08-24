/*
 * UpdateSpec.scala
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
class UpdateSpec extends FlatSpec with Matchers {

  import Update._

  "Update" should "create updates for specified from and to states" in {
    val b: Value = Value.Boolean(true)
    val n: Value = Value.Number(Math.PI)
    val s: Value = Value.String("hello")
    val t1: Fragment = Table(b -> n)
    val t2: Fragment = Table(b -> s)
    val t3: Fragment = Table(n -> b, s -> b)
    Update(b, b) shouldBe Copy(b)
    Update(n, n) shouldBe Copy(n)
    Update(s, s) shouldBe Copy(s)
    Update(t1, t1) shouldBe Copy(t1)
    Update(b, t1) shouldBe Replace(b, t1)
    Update(t2, n) shouldBe Replace(t2, n)
    Update(n, s) shouldBe Replace(n, s)
    Update(t1, t2) shouldBe Modify(t1.hash, b -> Replace(n, s))
    Update(t2, t3) shouldBe Modify(t2.hash, b -> Change.Remove(s), n -> Change.Add(b), s -> Change.Add(b))
    Update(t3, t1) shouldBe Modify(t3.hash, n -> Change.Remove(b), s -> Change.Remove(b), b -> Change.Add(n))
  }

  "A copy" should "act as a hashable copy of a fragment between tables" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    Copy(fa).hash shouldBe implicitly[Hasher].hashCopy(fa.hash)
    Copy(fb.hash).hash shouldBe implicitly[Hasher].hashCopy(fb.hash)
  }

  "A replace" should "act as a hashable replacement of a fragment in a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    Replace(fa, fb).hash shouldBe implicitly[Hasher].hashReplace(fa.hash, fb.hash)
    Replace(fb.hash, fa).hash shouldBe implicitly[Hasher].hashReplace(fb.hash, fa.hash)
  }

  "A modify" should "act as a hashable modification to the entries in a table" in {
    // Hash mismatch
    val ft = Table(Value.String("a") -> Value.Number())
    val tt = Table(Value.String("a") -> Value.Number(1.1))
    val r = Replace(tt.values.head, ft.values.head)
    Modify(tt.hash).hash shouldBe implicitly[Hasher].hashModify(tt.hash, Iterable.empty)
    Modify(ft, ft.keys.head -> r).hash shouldBe implicitly[Hasher].hashModify(ft.hash, Seq(ft.keys.head.hash, r.hash))
  }

}
