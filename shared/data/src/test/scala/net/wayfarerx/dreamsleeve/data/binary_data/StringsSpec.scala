/*
 * StringsSpec.scala
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
 * Test case for the string binary codec.
 */
class StringsSpec extends FlatSpec with Matchers {

  "A string value" should "encode to and decode from binary" in {
    val e = Value.String()
    val o = Value.String("1")
    val h = Value.String("hello")
    val codec = Codec[Value.String]
    codec.encode(e) shouldBe
      Attempt.successful(bin"0000000000000000")
    codec.encode(o) shouldBe
      Attempt.successful(bin"0000000000000001" ++ BitVector(o.value.getBytes(StandardCharsets.UTF_8)))
    codec.encode(h) shouldBe
      Attempt.successful(bin"0000000000000101" ++ BitVector(h.value.getBytes(StandardCharsets.UTF_8)))
    codec.decode(bin"0000000000000000") shouldBe
      Attempt.successful(DecodeResult(e, BitVector.empty))
    codec.decode(bin"0000000000000001" ++ BitVector(o.value.getBytes(StandardCharsets.UTF_8))) shouldBe
      Attempt.successful(DecodeResult(o, BitVector.empty))
    codec.decode(bin"0000000000000101" ++ BitVector(h.value.getBytes(StandardCharsets.UTF_8))) shouldBe
      Attempt.successful(DecodeResult(h, BitVector.empty))
  }

}
