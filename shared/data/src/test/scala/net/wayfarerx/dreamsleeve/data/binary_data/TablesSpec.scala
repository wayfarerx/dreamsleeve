/*
 * TablesSpec.scala
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
 * Test case for the table binary codec.
 */
class TablesSpec extends FlatSpec with Matchers {

  "A table" should "encode to and decode from binary" in {
    val e = Table()
    val t = Table(Value.Boolean() -> Value.Number(), Value.Boolean(true) -> Value.Number(1))
    val codec = Codec[Table]
    val entry = Codec[(Value, Fragment)]
    val entries = t.entries.toVector map entry.encode map (_.require) reduce (_ ++ _)
    codec.encode(e) shouldBe Attempt.successful(bin"0000000000000000")
    codec.encode(t) shouldBe Attempt.successful(bin"0000000000000010" ++ entries)
    codec.decode(bin"0000000000000000") shouldBe Attempt.successful(DecodeResult(e, BitVector.empty))
    codec.decode(bin"0000000000000010" ++ entries) shouldBe Attempt.successful(DecodeResult(t, BitVector.empty))
  }

}
