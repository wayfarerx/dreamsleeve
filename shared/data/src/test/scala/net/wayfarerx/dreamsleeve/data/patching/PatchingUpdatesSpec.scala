/*
 * PatchingUpdatesSpec.scala
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

import scala.collection.immutable.SortedSet

/**
 * Test case for the change patching implementations.
 */
class PatchingUpdatesSpec extends FlatSpec with Matchers {

  import Update._

  "An update" should "patch as the concrete type does" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Table(Value.String("a") -> Value.Number())
    val a = Copy(fa)
    val b = Replace(fa.hash, fb)
    val c = Modify(fb.hash, Value.String("a") -> Replace(Value.Number(), Value.Number(1)))
    (a: Update).patch(fa) shouldBe a.patch(fa)
    (b: Update).patch(fa) shouldBe b.patch(fa)
    (c: Update).patch(fb) shouldBe c.patch(fb)
  }

  "A copy" should "patch a copy of a fragment between tables" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Copy(fa)
    val b = Copy(fb.hash)
    a.patch(fa) shouldBe Right(fa)
    a.patch(fb) shouldBe Left(Vector(Problems.HashMismatch(fa.hash, fb.hash)))
    b.patch(fa) shouldBe Left(Vector(Problems.HashMismatch(fb.hash, fa.hash)))
    b.patch(fb) shouldBe Right(fb)
  }

  "A replace" should "patch a replacement of a fragment in a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Replace(fa, fb)
    val b = Replace(fb.hash, fa)
    a.patch(fa) shouldBe Right(fb)
    a.patch(fb) shouldBe Left(Vector(Problems.HashMismatch(fa.hash, fb.hash)))
    b.patch(fa) shouldBe Left(Vector(Problems.HashMismatch(fb.hash, fa.hash)))
    b.patch(fb) shouldBe Right(fa)
  }

  "A modify" should "patch a modification to the entries in a table" in {
    // Hash mismatch
    val ft = Table(Value.String("a") -> Value.Number())
    val tt = Table(Value.String("a") -> Value.Number(1.1))
    val a = Modify(ft, ft.keys.head -> Replace(tt.values.head, ft.values.head))
    a.patch(tt) shouldBe Left(Vector(Problems.HashMismatch(ft.hash, tt.hash)))
    // Type mismatch
    val fv = Value.String("a")
    val b = Modify(fv.hash)
    b.patch(fv) shouldBe Left(Vector(Problems.TypeMismatch(fv)))
    // Hash & type mismatch
    val c = Modify(ft.hash)
    c.patch(fv) shouldBe Left(Vector(Problems.HashMismatch(ft.hash, fv.hash), Problems.TypeMismatch(fv)))
    // Missing change keys
    val et = Table()
    val d = Modify(ft)
    d.patch(ft) shouldBe Left(Vector(Problems.MissingChangeKeys(SortedSet(ft.keys.head))))
    // Unexpected entry
    val e = Modify(ft, ft.keys.head -> Change.Add(tt.values.head))
    e.patch(ft) shouldBe Left(Vector(Problems.UnexpectedEntry(ft.keys.head)))
    // Add an entry
    val f = Modify(et, tt.keys.head -> Change.Add(tt.values.head))
    f.patch(et) shouldBe Right(tt)
    // Missing entry
    val g = Modify(et, tt.keys.head -> Change.Remove(tt.values.head))
    g.patch(et) shouldBe Left(Vector(Problems.MissingEntry(tt.keys.head)))
    // Remove an entry
    val h = Modify(ft, ft.keys.head -> Change.Remove(ft.values.head))
    h.patch(ft) shouldBe Right(et)
    // Copy an entry
    val i = Modify(tt, tt.keys.head -> Copy(tt.values.head))
    i.patch(tt) shouldBe Right(tt)
    // Replace an entry
    val j = Modify(ft, ft.keys.head -> Replace(ft.values.head, tt.values.head))
    j.patch(ft) shouldBe Right(tt)
    // Modify an entry
    val x = Value.String("x")
    val ftt = Table(x -> ft)
    val ttt = Table(x -> tt)
    val k = Modify(ftt, x -> Modify(ft, ft.keys.head -> Replace(ft.values.head, tt.values.head)))
    k.patch(ftt) shouldBe Right(ttt)
    // Hash, key & entries mismatch
    val ft2 = ft.copy(entries = ft.entries + (Value.String("c") -> Value.Boolean()))
    val l = Modify(et.hash,
      Value.String("a") -> Change.Add(ft.values.head),
      Value.String("b") -> Change.Remove(ft.values.head))
    l.patch(ft2) shouldBe Left(Vector(
      Problems.HashMismatch(et.hash, ft2.hash),
      Problems.MissingChangeKeys(SortedSet(Value.String("c"))),
      Problems.UnexpectedEntry(Value.String("a")),
      Problems.MissingEntry(Value.String("b"))
    ))
  }

}
