/*
 * AddsSpec.scala
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
package binary_data

import org.scalatest._
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits._

/**
 * Test case for the add binary codec.
 */
class AddsSpec extends FlatSpec with Matchers {

  "An add operation" should "encode to and decode from binary" in {
    val f: Fragment = Value.Boolean()
    val t: Fragment = Value.Boolean(true)
    val fr = Change.Add(f)
    val tr = Change.Add(t)
    val codec = Codec[Change.Add]
    val fragments = Codec[Fragment]
    codec.encode(fr) shouldBe Attempt.successful(fragments.encode(f).require)
    codec.encode(tr) shouldBe Attempt.successful(fragments.encode(t).require)
    codec.decode(fragments.encode(f).require) shouldBe Attempt.successful(DecodeResult(fr, BitVector.empty))
    codec.decode(fragments.encode(t).require) shouldBe Attempt.successful(DecodeResult(tr, BitVector.empty))
  }

}
