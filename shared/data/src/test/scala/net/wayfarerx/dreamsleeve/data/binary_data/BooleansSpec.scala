/*
 * BooleansSpec.scala
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
 * Test case for the boolean binary codec.
 */
class BooleansSpec extends FlatSpec with Matchers {

  "A boolean value" should "encode to and decode from binary" in {
    val f = Value.Boolean()
    val t = Value.Boolean(true)
    val codec = Codec[Value.Boolean]
    codec.encode(f) shouldBe Attempt.successful(bin"0")
    codec.encode(t) shouldBe Attempt.successful(bin"1")
    codec.decode(bin"0") shouldBe Attempt.successful(DecodeResult(f, BitVector.empty))
    codec.decode(bin"1") shouldBe Attempt.successful(DecodeResult(t, BitVector.empty))
  }

}
