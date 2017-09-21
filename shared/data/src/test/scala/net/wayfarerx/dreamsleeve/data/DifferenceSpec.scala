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
  import Hashable.HashTask

  "A create" should "act as a hashable creation of a document" in {
    val da = Document("e", Table())
    val db = Document("f", Table())
    val a = Create(da)
    val b = Create(db)
    a == a shouldBe true
    a == b shouldBe false
    a == ("Hello": Any) shouldBe false
    a.toString shouldBe s"Create($da)"
    b.toString shouldBe s"Create($db)"
    a.hash shouldBe HashTask.hash(Create.Header, da.hash).foldMap(HashTask.interpreter())
    Create.unapply(a) shouldBe Some(da)
    Difference.unapply(a) shouldBe true
  }

  "A revise" should "verify the hash of a document and apply a change" in {
    val ta = Table(Value.String("a") -> Value.Number(1))
    val tb = Table(Value.String("a") -> Value.Number(2))
    val da = Document("e", ta)
    val db = Document("g", tb)
    val a = Revise(da, db.title, Update(ta, tb))
    val b = Revise(db, da.title, Update(tb, ta))
    a == a shouldBe true
    a == b shouldBe false
    a.toString shouldBe s"Revise(${da.hash},${db.title},${Update(ta, tb)})"
    b.toString shouldBe s"Revise(${db.hash},${da.title},${Update(tb, ta)})"
    a.hash shouldBe HashTask.hash(Revise.Header, da.hash, db.title, a.update.hash).foldMap(HashTask.interpreter())
    Revise.unapply(a) shouldBe Some((da.hash, db.title, a.update))
    Difference.unapply(a) shouldBe true
  }

  "A delete" should "verify the hash of a document" in {
    val da = Document("e", Table())
    val db = Document("f", Table())
    val a = Delete(da)
    val b = Delete(db.hash)
    a == a shouldBe true
    a == b shouldBe false
    a.toString shouldBe s"Delete(${da.hash})"
    b.toString shouldBe s"Delete(${db.hash})"
    a.hash shouldBe HashTask.hash(Delete.Header, da.hash).foldMap(HashTask.interpreter())
    Delete.unapply(a) shouldBe Some(da.hash)
    Difference.unapply(a) shouldBe true
  }

}
