/*
 * UpdatesSpec.scala
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
package diff_data

import org.scalatest._

/**
 * Test case for the update diffing implementation.
 */
class UpdatesSpec extends FlatSpec with Matchers {

  "Update" should "detect the differences between fragments" in {
    val b: Value = Value.Boolean(true)
    val n: Value = Value.Number(Math.PI)
    val s: Value = Value.String("hello")
    val t1: Fragment = Table(b -> n)
    val t2: Fragment = Table(b -> s)
    val t3: Fragment = Table(n -> b, s -> b)
    Update(b, b) shouldBe Update.Copy(b)
    Update(n, n) shouldBe Update.Copy(n)
    Update(s, s) shouldBe Update.Copy(s)
    Update(t1, t1) shouldBe Update.Copy(t1)
    Update(b, t1) shouldBe Update.Replace(b, t1)
    Update(t2, n) shouldBe Update.Replace(t2, n)
    Update(n, s) shouldBe Update.Replace(n, s)
    Update(t1, t2) shouldBe Update.Modify(t1.hash, b -> Update.Replace(n, s))
    Update(t2, t3) shouldBe Update.Modify(t2.hash, b -> Change.Remove(s), n -> Change.Add(b), s -> Change.Add(b))
    Update(t3, t1) shouldBe Update.Modify(t3.hash, n -> Change.Remove(b), s -> Change.Remove(b), b -> Change.Add(n))
  }

}
