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

  import Hashable.HashTask

  import Update._

  "A copy" should "act as a hashable copy of a fragment between tables" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    Copy(fa).hash shouldBe HashTask.hash(Update.Copy.Header, fa.hash).foldMap(HashTask.interpreter())
    Copy(fb.hash).hash shouldBe HashTask.hash(Update.Copy.Header, fb.hash).foldMap(HashTask.interpreter())
    Copy.unapply(Copy(fa)) shouldBe Some(fa.hash)
    Update.unapply(Copy(fb)) shouldBe true
  }

  "A replace" should "act as a hashable replacement of a fragment in a table" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Value.String("b")
    Replace(fa, fb).hash shouldBe HashTask.hash(Update.Replace.Header, fa.hash, fb.hash).foldMap(HashTask.interpreter())
    Replace(fb.hash, fa).hash shouldBe HashTask.hash(Update.Replace.Header, fb.hash, fa.hash).foldMap(HashTask.interpreter())
    Replace.unapply(Replace(fa, fb)) shouldBe Some((fa.hash, fb))
    Update.unapply(Replace(fb.hash, fa)) shouldBe true
  }

  "A modify" should "act as a hashable modification to the entries in a table" in {
    val ft = Table(Value.String("a") -> Value.Number())
    val tt = Table(Value.String("a") -> Value.Number(1.1))
    val r = Replace(tt.values.head, ft.values.head)
    Modify(tt.hash).hash shouldBe HashTask.hash(Update.Modify.Header, tt.hash, Iterable.empty[Hash]).foldMap(HashTask.interpreter())
    Modify(ft, ft.keys.head -> r).hash shouldBe HashTask.hash(Update.Modify.Header, ft.hash, Seq(ft.keys.head.hash, r.hash)).foldMap(HashTask.interpreter())
    Modify.unapply(Modify(ft, ft.keys.head -> r)) shouldBe Some((ft.hash, SortedMap(ft.keys.head -> r)))
    Update.unapply(Modify(tt.hash)) shouldBe true
  }

}
