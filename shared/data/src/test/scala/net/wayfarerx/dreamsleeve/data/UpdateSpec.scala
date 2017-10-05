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

import scala.collection.immutable.SortedMap

/**
 * Test case for the change implementations.
 */
class UpdateSpec extends FlatSpec with Matchers {

  val generator = Hash.Generator()

  import Update._

  "A copy" should "act as a hashable copy of a fragment between tables" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Copy(fa)
    val b = Copy(fb.hash)
    a == a shouldBe true
    a == b shouldBe false
    a == ("Hi": Any) shouldBe false
    a.toString shouldBe s"Copy(${fa.hash})"
    b.toString shouldBe s"Copy(${fb.hash})"
    a.hash shouldBe generator.hash(Update.Copy.Header, fa.hash)
    b.hash shouldBe generator.hash(Update.Copy.Header, fb.hash)
    Copy.unapply(a) shouldBe Some(fa.hash)
    Update.unapply(Copy(fb)) shouldBe true
  }

  "A replace" should "act as a hashable replacement of a fragment in a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Replace(fa, fb)
    val b = Replace(fb.hash, fa)
    a == a shouldBe true
    a == b shouldBe false
    a == ("Hi": Any) shouldBe false
    a.toString shouldBe s"Replace(${fa.hash},$fb)"
    b.toString shouldBe s"Replace(${fb.hash},$fa)"
    a.hash shouldBe generator.hash(Update.Replace.Header, fa.hash, fb.hash)
    b.hash shouldBe generator.hash(Update.Replace.Header, fb.hash, fa.hash)
    Replace.unapply(a) shouldBe Some((fa.hash, fb))
    Update.unapply(b) shouldBe true
  }

  "A modify" should "act as a hashable modification to the entries in a table" in {
    val ft = Table(Value.String("a") -> Value.Number())
    val tt = Table(Value.String("a") -> Value.Number(1.1))
    val r = Replace(tt.values.head, ft.values.head)
    val a = Modify(tt.hash)
    val b = Modify(ft, ft.keys.head -> r)
    b == b shouldBe true
    a == b shouldBe false
    a == ("Hi": Any) shouldBe false
    b == Modify(ft, ft.keys.head -> Copy(Value.Number())) shouldBe false
    a.toString shouldBe s"Modify(${tt.hash},{})"
    b.toString shouldBe s"Modify(${ft.hash},{${ft.keys.head}=$r})"
    a.hash shouldBe generator.hash(Update.Modify.Header, tt.hash, Iterable.empty[Hash])
    b.hash shouldBe generator.hash(Update.Modify.Header, ft.hash, Seq(ft.keys.head.hash, r.hash))
    Modify.unapply(b) shouldBe Some((ft.hash, SortedMap(ft.keys.head -> r)))
    Update.unapply(a) shouldBe true
  }

  it should "enforce stack safety for all recursive operations" in {
    StackSpec.StackOverflowModify1.equals(StackSpec.StackOverflowModify2) shouldBe false
    StackSpec.StackOverflowModify1.hash shouldNot be(null)
    StackSpec.StackOverflowModify1.toString shouldNot be(null)
  }

}
