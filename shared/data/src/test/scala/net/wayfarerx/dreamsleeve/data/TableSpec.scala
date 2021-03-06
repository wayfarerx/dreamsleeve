/*
 * TableSpec.scala
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

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * Test case for the table implementation.
 */
class TableSpec extends FlatSpec with Matchers {

  val generator = Hash.Generator()

  "A table" should "act as a hashable fragment that contains values and fragments" in {
    val e = Table()
    val o = Table(Value.Number(1) -> Value.String("1"))
    val t = Table(
      Value.Number(1) -> Value.String("1"),
      Value.Number(2) -> Value.String("2"),
      Value.Number(3) -> Value.String("3"))
    e == e shouldBe true
    e == o shouldBe false
    e == ("Hi": Any) shouldBe false
    t == t.copy(entries = t.entries + (Value.Number(3) -> Value.String("4"))) shouldBe false
    e.toString shouldBe "Table()"
    o.toString shouldBe "Table(Number(1)=String(1))"
    t.toString shouldBe "Table(Number(1)=String(1),Number(2)=String(2),Number(3)=String(3))"
    e.hash shouldBe generator.hash(Table.Header, Iterable.empty[Hash])
    o.hash shouldBe generator.hash(Table.Header, Seq(
      Value.Number(1).hash, Value.String("1").hash))
    t.hash shouldBe generator.hash(Table.Header, Seq(
      Value.Number(1).hash, Value.String("1").hash,
      Value.Number(2).hash, Value.String("2").hash,
      Value.Number(3).hash, Value.String("3").hash))
    Table.unapply(o) shouldBe Some(SortedMap[Value, Fragment](Value.Number(1) -> Value.String("1")))
  }

  it should "publish a subset of the underlying map interface" in {
    val t = Table(SortedMap[Value, Fragment](
      Value.Number(1) -> Value.String("1"),
      Value.Number(2) -> Value.String("2"),
      Value.Number(3) -> Value.String("3")))
    t.keys shouldBe SortedSet[Value](Value.Number(1), Value.Number(2), Value.Number(3))
    t.values.toVector shouldBe Vector[Fragment](Value.String("1"), Value.String("2"), Value.String("3"))
    t(Value.Number(2)) shouldBe Value.String("2")
    t.get(Value.Number(3)) shouldBe Some(Value.String("3"))
    t.get(Value.Number(4)) shouldBe None
  }

  it should "enforce stack safety for all recursive operations" in {
    StackSpec.StackOverflowTable1.equals(StackSpec.StackOverflowTable2) shouldBe false
    StackSpec.StackOverflowTable1.hash shouldNot be(null)
    StackSpec.StackOverflowTable1.toString shouldNot be(null)
  }

}
