/*
 * DeletesSpec.scala
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
 * Test case for the delete binary codec.
 */
class DeletesSpec extends FlatSpec with Matchers {

  "A delete operation" should "encode to and decode from binary" in {
    val d = Document("hi", Value.Boolean())
    val r = Difference.Delete(d)
    val dh = d.hash.toBytes
    val codec = Codec[Difference.Delete]
    codec.encode(r) shouldBe Attempt.successful(BitVector(dh))
    codec.decode(BitVector(dh)) shouldBe Attempt.successful(DecodeResult(r, BitVector.empty))
  }

}
