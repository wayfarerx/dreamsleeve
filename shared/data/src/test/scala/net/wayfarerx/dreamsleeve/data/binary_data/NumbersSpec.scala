/*
 * NumbersSpec.scala
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
 * Test case for the number binary codec.
 */
class NumbersSpec extends FlatSpec with Matchers {

  "A number value" should "encode to and decode from binary" in {
    val z = Value.Number()
    val o = Value.Number(1)
    val g = Value.Number(1.21f)
    val p = Value.Number(Math.PI)
    val codec = Codec[Value.Number]
    codec.encode(z) shouldBe
      Attempt.successful(bin"0000000000")
    codec.encode(o) shouldBe
      Attempt.successful(bin"0000000001")
    codec.encode(g) shouldBe
      Attempt.successful(bin"01" ++ BitVector.fromInt(java.lang.Float.floatToIntBits(1.21f)))
    codec.encode(p) shouldBe
      Attempt.successful(bin"10" ++ BitVector.fromLong(java.lang.Double.doubleToLongBits(Math.PI)))
    codec.decode(bin"0000000000") shouldBe
      Attempt.successful(DecodeResult(z, BitVector.empty))
    codec.decode(bin"0000000001") shouldBe
      Attempt.successful(DecodeResult(o, BitVector.empty))
    codec.decode(bin"01" ++ BitVector.fromInt(java.lang.Float.floatToIntBits(1.21f))) shouldBe
      Attempt.successful(DecodeResult(g, BitVector.empty))
    codec.decode(bin"10" ++ BitVector.fromLong(java.lang.Double.doubleToLongBits(Math.PI))) shouldBe
      Attempt.successful(DecodeResult(p, BitVector.empty))
  }


}
