/*
 * PatchModifySpec.scala
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

import collection.immutable.SortedSet

import org.scalatest._

/**
 * Test case for the modify patching implementation.
 */
class PatchModifySpec extends FlatSpec with Matchers {

  "A modify" should "patch a modification to the entries in a table" in {
    // Hash mismatch
    val ft = Table(Value.String("a") -> Value.Number())
    val tt = Table(Value.String("a") -> Value.Number(1.1))
    val a = Update.Modify(ft, ft.keys.head -> Update.Replace(tt.values.head, ft.values.head))
    a.patch(tt) shouldBe Left(PatchProblem.HashMismatch(ft.hash, tt.hash))
    // Type mismatch
    val fv = Value.String("a")
    val b = Update.Modify(fv.hash)
    b.patch(fv) shouldBe Left(PatchProblem.TypeMismatch(fv))
    // Hash & type mismatch
    val c = Update.Modify(ft.hash)
    c.patch(fv) shouldBe Left(PatchProblem.HashMismatch(ft.hash, fv.hash))
    // Missing change keys
    val et = Table()
    val d = Update.Modify(ft)
    d.patch(ft) shouldBe Left(PatchProblem.MismatchedEntries(SortedSet(ft.keys.head)))
    // Unexpected entry
    val e = Update.Modify(ft, ft.keys.head -> Change.Add(tt.values.head))
    e.patch(ft) shouldBe Left(PatchProblem.UnexpectedEntry(ft.keys.head))
    // Add an entry
    val f = Update.Modify(et, tt.keys.head -> Change.Add(tt.values.head))
    f.patch(et) shouldBe Right(tt)
    // Missing entry
    val g = Update.Modify(et, tt.keys.head -> Change.Remove(tt.values.head))
    g.patch(et) shouldBe Left(PatchProblem.MissingEntry(tt.keys.head))
    // Remove an entry
    val h = Update.Modify(ft, ft.keys.head -> Change.Remove(ft.values.head))
    h.patch(ft) shouldBe Right(et)
    // Copy an entry
    val i = Update.Modify(tt, tt.keys.head -> Update.Copy(tt.values.head))
    i.patch(tt) shouldBe Right(tt)
    // Replace an entry
    val j = Update.Modify(ft, ft.keys.head -> Update.Replace(ft.values.head, tt.values.head))
    j.patch(ft) shouldBe Right(tt)
    // Modify an entry
    val x = Value.String("x")
    val ftt = Table(x -> ft)
    val ttt = Table(x -> tt)
    val k = Update.Modify(ftt, x -> Update.Modify(ft, ft.keys.head -> Update.Replace(ft.values.head, tt.values.head)))
    k.patch(ftt) shouldBe Right(ttt)
    // Hash, key & entries mismatch
    val ft2 = ft.copy(entries = ft.entries + (Value.String("c") -> Value.Boolean()))
    val l = Update.Modify(et.hash,
      Value.String("a") -> Change.Add(ft.values.head),
      Value.String("b") -> Change.Remove(ft.values.head))
    l.patch(ft2) shouldBe Left(PatchProblem.HashMismatch(et.hash, ft2.hash))
  }

}
