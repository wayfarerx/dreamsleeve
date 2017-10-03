/*
 * RevisesSpec.scala
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

import java.nio.charset.StandardCharsets

import org.scalatest._
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits._

/**
 * Test case for the revise binary codec.
 */
class RevisesSpec extends FlatSpec with Matchers {

  "A revise operation" should "encode to and decode from binary" in {
    val d = Document("hello", Value.Boolean())
    val r = Difference.Revise(d, "hi", Update.Replace(Value.Boolean(), Value.Boolean(true)))
    val codec = Codec[Difference.Revise]
    val updates = Codec[Update]
    codec.encode(r) shouldBe
      Attempt.successful(BitVector(d.hash.toBytes) ++
        bin"0000000000000010" ++
        BitVector("hi".getBytes(StandardCharsets.UTF_8)) ++
        updates.encode(Update.Replace(Value.Boolean(), Value.Boolean(true))).require)
    codec.decode(BitVector(d.hash.toBytes) ++
      bin"0000000000000010" ++
      BitVector("hi".getBytes(StandardCharsets.UTF_8)) ++
      updates.encode(Update.Replace(Value.Boolean(), Value.Boolean(true))).require) shouldBe
      Attempt.successful(DecodeResult(r, BitVector.empty))
  }

}
