/*
 * BinaryContextSpec.scala
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

package net.wayfarerx.dreamsleeve.io

import org.scalatest._

/**
 * Test case for the binary context support class.
 */
class BinaryContextSpec extends FlatSpec with Matchers {

  "The binary context support class" should "manage a cached byte buffer" in {
    val context = new BinaryContext.Support {}
    context.acquireBytes(1).remaining shouldBe 1
    context.acquireBytes(3).remaining shouldBe 3
    context.acquireBytes(5).remaining shouldBe 5
    context.acquireBytes(7).remaining shouldBe 7
    context.acquireBytes(11).remaining shouldBe 11
    context.acquireBytes(13).remaining shouldBe 13
    context.acquireBytes(17).remaining shouldBe 17
    context.acquireBytes(1) eq context.acquireBytes(17) shouldBe true
    // Initialize the companion object for coverage
    BinaryContext.toString
  }

}
