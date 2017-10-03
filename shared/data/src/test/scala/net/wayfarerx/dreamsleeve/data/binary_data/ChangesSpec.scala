/*
 * ChangesSpec.scala
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
 * Test case for the change binary codec.
 */
class ChangesSpec extends FlatSpec with Matchers {

  "A change operation" should "encode to and decode from binary" in {
    val c = Update.Copy(Value.Boolean())
    val bh = Value.Boolean().hash.toBytes
    val ff: Fragment = Value.Boolean()
    val tf: Fragment = Value.Boolean(true)
    val fh = ff.hash.toBytes
    val r = Update.Replace(ff, tf)
    val t = Table(Value.Boolean() -> Value.Number())
    val m = Update.Modify(t, Value.Boolean() -> Update.Copy(Value.Number(1)))
    val a = Change.Add(Value.Boolean())
    val rm = Change.Remove(Value.Boolean())
    val codec = Codec[Change]
    val fragments = Codec[Fragment]
    val entries = Codec[(Value, Change)]
    val changes = m.changes.toVector map entries.encode map (_.require) reduce (_ ++ _)
    codec.encode(c) shouldBe
      Attempt.successful(bin"001" ++ BitVector(bh))
    codec.encode(r) shouldBe
      Attempt.successful(bin"010" ++ BitVector(fh) ++ fragments.encode(tf).require)
    codec.encode(m) shouldBe
      Attempt.successful(bin"011" ++ BitVector(t.hash.toBytes) ++ bin"0000000000000001" ++ changes)
    codec.encode(a) shouldBe
      Attempt.successful(bin"100" ++ fragments.encode(Value.Boolean()).require)
    codec.encode(rm) shouldBe
      Attempt.successful(bin"101" ++ BitVector(bh))
    codec.decode(bin"001" ++ BitVector(bh)) shouldBe
      Attempt.successful(DecodeResult(c, BitVector.empty))
    codec.decode(bin"010" ++ BitVector(fh) ++ fragments.encode(tf).require) shouldBe
      Attempt.successful(DecodeResult(r, BitVector.empty))
    codec.decode(bin"011" ++ BitVector(t.hash.toBytes) ++ bin"0000000000000001" ++ changes) shouldBe
      Attempt.successful(DecodeResult(m, BitVector.empty))
    codec.decode(bin"100" ++ fragments.encode(Value.Boolean()).require) shouldBe
      Attempt.successful(DecodeResult(a, BitVector.empty))
    codec.decode(bin"101" ++ BitVector(bh)) shouldBe
      Attempt.successful(DecodeResult(rm, BitVector.empty))
  }

}
