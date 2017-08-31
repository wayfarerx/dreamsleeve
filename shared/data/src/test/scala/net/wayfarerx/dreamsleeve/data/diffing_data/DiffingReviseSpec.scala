/*
 * DiffingReviseSpec.scala
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
package diffing_data

import org.scalatest._

/**
 * Test case for the revise diffing implementation.
 */
class DiffingReviseSpec extends FlatSpec with Matchers {

  "Revise" should "detect the differences between tables" in {
    val t1 = Table(Value.String("a") -> Value.Number(1))
    val t2 = Table(Value.String("a") -> Value.Number(2))
    val d1 = Document("e", t1)
    val d2 = Document("g", t2)
    Difference.Revise(d1, d2) shouldBe Difference.Revise(d1.hash, d2.title, Update(t1, t2))

  }

}
