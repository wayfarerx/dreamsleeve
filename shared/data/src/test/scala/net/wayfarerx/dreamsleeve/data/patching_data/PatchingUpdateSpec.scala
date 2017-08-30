/*
 * PatchingUpdateSpec.scala
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
package patching_data

import org.scalatest._

/**
 * Test case for the update patching implementation.
 */
class PatchingUpdateSpec extends FlatSpec with Matchers {

  "An update" should "patch as the concrete type does" in {
    val fa: Fragment = Value.String("a")
    val fb: Fragment = Table(Value.String("a") -> Value.Number())
    val a = Update.Copy(fa)
    val b = Update.Replace(fa.hash, fb)
    val c = Update.Modify(fb.hash, Value.String("a") -> Update.Replace(Value.Number(), Value.Number(1)))
    (a: Update).patch(fa) shouldBe a.patch(fa)
    (b: Update).patch(fa) shouldBe b.patch(fa)
    (c: Update).patch(fb) shouldBe c.patch(fb)
  }

}
