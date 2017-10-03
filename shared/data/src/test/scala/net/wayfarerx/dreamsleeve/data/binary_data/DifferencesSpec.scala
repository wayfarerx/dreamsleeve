/*
 * DifferencesSpec.scala
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
 * Test case for the difference binary codec.
 */
class DifferencesSpec extends FlatSpec with Matchers {

  "A difference operation" should "encode to and decode from binary" in {
    val c = Difference.Create(Document("hi", Value.Boolean()))
    val r = Difference.Revise(c.document, "hello", Update.Replace(Value.Boolean(), Value.Boolean(true)))
    val d = Difference.Delete(Document("hello", Value.Boolean(true)))
    val codec = Codec[Difference]
    codec.encode(c) shouldBe
      Attempt.successful(bin"01" ++ Codec[Difference.Create].encode(c).require)
    codec.encode(r) shouldBe
      Attempt.successful(bin"10" ++ Codec[Difference.Revise].encode(r).require)
    codec.encode(d) shouldBe
      Attempt.successful(bin"11" ++ Codec[Difference.Delete].encode(d).require)
    codec.decode(bin"01" ++ Codec[Difference.Create].encode(c).require) shouldBe
      Attempt.successful(DecodeResult(c, BitVector.empty))
    codec.decode(bin"10" ++ Codec[Difference.Revise].encode(r).require) shouldBe
      Attempt.successful(DecodeResult(r, BitVector.empty))
    codec.decode(bin"11" ++ Codec[Difference.Delete].encode(d).require) shouldBe
      Attempt.successful(DecodeResult(d, BitVector.empty))
  }

}
