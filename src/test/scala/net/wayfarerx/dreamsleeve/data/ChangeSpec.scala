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
import cats.data._
import Validated.{invalid, valid}

import scala.collection.immutable.SortedSet

/**
 * Test case for the change implementations.
 */
class ChangeSpec extends FlatSpec with Matchers {

  import Change._

  implicit val ctx: Problem.Context = Problem.Context(Vector())

  "An add" should "act as a hashable addition of a fragment to a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Add(fa)
    val b = Add(fb)
    a.hash shouldBe implicitly[Hasher].hashAdd(fa.hash)
    b.hash shouldBe implicitly[Hasher].hashAdd(fb.hash)
    a() shouldBe valid(fa)
    b() shouldBe valid(fb)
  }

  "A remove" should "act as a hashable removal of a fragment from a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Remove(fa)
    val b = Remove(fb.hash)
    a.hash shouldBe implicitly[Hasher].hashRemove(fa.hash)
    b.hash shouldBe implicitly[Hasher].hashRemove(fb.hash)
    a(fa) shouldBe valid(())
    a(fb) shouldBe invalid(Problem.List.of(Problem.HashMismatch(fa.hash, fb.hash)))
    b(fa) shouldBe invalid(Problem.List.of(Problem.HashMismatch(fb.hash, fa.hash)))
    b(fb) shouldBe valid(())
  }

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
    Update(t2, t3) shouldBe Modify(t2.hash, b -> Remove(s), n -> Add(b), s -> Add(b))
    Update(t3, t1) shouldBe Modify(t3.hash, n -> Remove(b), s -> Remove(b), b -> Add(n))
  }

  "A copy" should "act as a hashable copy of a fragment between tables" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Copy(fa)
    val b = Copy(fb.hash)
    a.hash shouldBe implicitly[Hasher].hashCopy(fa.hash)
    b.hash shouldBe implicitly[Hasher].hashCopy(fb.hash)
    a(fa) shouldBe valid(fa)
    a(fb) shouldBe invalid(Problem.List.of(Problem.HashMismatch(fa.hash, fb.hash)))
    b(fa) shouldBe invalid(Problem.List.of(Problem.HashMismatch(fb.hash, fa.hash)))
    b(fb) shouldBe valid(fb)
  }

  "A replace" should "act as a hashable replacement of a fragment in a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    val a = Replace(fa, fb)
    val b = Replace(fb.hash, fa)
    a.hash shouldBe implicitly[Hasher].hashReplace(fa.hash, fb.hash)
    b.hash shouldBe implicitly[Hasher].hashReplace(fb.hash, fa.hash)
    a(fa) shouldBe valid(fb)
    a(fb) shouldBe invalid(Problem.List.of(Problem.HashMismatch(fa.hash, fb.hash)))
    b(fa) shouldBe invalid(Problem.List.of(Problem.HashMismatch(fb.hash, fa.hash)))
    b(fb) shouldBe valid(fa)
  }

  "A modify" should "act as a hashable modification to the entries in a table" in {
    // Hash mismatch
    val ft = Table(Value.String("a") -> Value.Number())
    val tt = Table(Value.String("a") -> Value.Number(1.1))
    val a = Modify(ft, ft.keys.head -> Replace(tt.values.head, ft.values.head))
    a(tt) shouldBe invalid(Problem.List.of(Problem.HashMismatch(ft.hash, tt.hash)))
    // Type mismatch
    val fv = Value.String("a")
    val b = Modify(fv.hash)
    b(fv) shouldBe invalid(Problem.List.of(Problem.TypeMismatch(fv)))
    // Hash & type mismatch
    val c = Modify(ft.hash)
    c(fv) shouldBe invalid(Problem.List.of(Problem.HashMismatch(ft.hash, fv.hash), Problem.TypeMismatch(fv)))
    // Missing change keys
    val et = Table()
    val d = Modify(ft)
    d(ft) shouldBe invalid(Problem.List.of(Problem.MissingChangeKeys(SortedSet(ft.keys.head))))
    // Unexpected entry
    val e = Modify(ft, ft.keys.head -> Add(tt.values.head))
    e(ft) shouldBe invalid(Problem.List.of(Problem.UnexpectedEntry(ft.keys.head)))
    // Add an entry
    val f = Modify(et, tt.keys.head -> Add(tt.values.head))
    f(et) shouldBe valid(tt)
    // Missing entry
    val g = Modify(et, tt.keys.head -> Remove(tt.values.head))
    g(et) shouldBe invalid(Problem.List.of(Problem.MissingEntry(tt.keys.head)))
    // Remove an entry
    val h = Modify(ft, ft.keys.head -> Remove(ft.values.head))
    h(ft) shouldBe valid(et)
    // Copy an entry
    val i = Modify(tt, tt.keys.head -> Copy(tt.values.head))
    i(tt) shouldBe valid(tt)
    // Replace an entry
    val j = Modify(ft, ft.keys.head -> Replace(ft.values.head, tt.values.head))
    j(ft) shouldBe valid(tt)
    // Modify an entry
    val x = Value.String("x")
    val ftt = Table(x -> ft)
    val ttt = Table(x -> tt)
    val k = Modify(ftt, x -> Modify(ft, ft.keys.head -> Replace(ft.values.head, tt.values.head)))
    k(ftt) shouldBe valid(ttt)
    // Hash, key & entries mismatch
    val ft2 = ft.copy(entries = ft.entries + (Value.String("c") -> Value.Boolean()))
    val l = Modify(et.hash, Value.String("a") -> Add(ft.values.head), Value.String("b") -> Remove(ft.values.head))
    l(ft2) shouldBe invalid(Problem.List.of(
      Problem.HashMismatch(et.hash, ft2.hash),
      Problem.MissingChangeKeys(SortedSet(Value.String("c"))),
      Problem.UnexpectedEntry(Value.String("a")),
      Problem.MissingEntry(Value.String("b"))
    ))
  }

}
