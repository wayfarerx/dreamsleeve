/*
 * ModifiesSpec.scala
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
 * Test case for the modify binary codec.
 */
class ModifiesSpec extends FlatSpec with Matchers {

  "A modify operation" should "encode to and decode from binary" in {
    val t = Table(Value.Boolean() -> Value.Number())
    val m = Update.Modify(t, Value.Boolean() -> Update.Copy(Value.Number(1)))
    val codec = Codec[Update.Modify]
    val entry = Codec[(Value, Change)]
    val changes = m.changes.toVector map entry.encode map (_.require) reduce (_ ++ _)
    codec.encode(m) shouldBe
      Attempt.successful(BitVector(t.hash.toBytes) ++ bin"0000000000000001" ++ changes)
    codec.decode(BitVector(t.hash.toBytes) ++ bin"0000000000000001" ++ changes) shouldBe
      Attempt.successful(DecodeResult(m, BitVector.empty))
  }

}
