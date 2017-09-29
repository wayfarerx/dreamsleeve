/*
 * ValuesSpec.scala
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
 * Test case for the value binary codec.
 */
class ValuesSpec extends FlatSpec with Matchers {

  "All values" should "encode to and decode from binary" in {
    val b = Value.Boolean(true)
    val n = Value.Number(1)
    val s = Value.String("1")
    val codec = Codec[Value]
    codec.encode(b) shouldBe
      Attempt.successful(bin"001")
    codec.encode(n) shouldBe
      Attempt.successful(bin"010000000001")
    codec.encode(s) shouldBe
      Attempt.successful(bin"100000000000000001" ++ BitVector(s.value.getBytes(StandardCharsets.UTF_8)))
    codec.decode(bin"001") shouldBe
      Attempt.successful(DecodeResult(b, BitVector.empty))
    codec.decode(bin"010000000001") shouldBe
      Attempt.successful(DecodeResult(n, BitVector.empty))
    codec.decode(bin"100000000000000001" ++ BitVector(s.value.getBytes(StandardCharsets.UTF_8))) shouldBe
      Attempt.successful(DecodeResult(s, BitVector.empty))
  }

}
