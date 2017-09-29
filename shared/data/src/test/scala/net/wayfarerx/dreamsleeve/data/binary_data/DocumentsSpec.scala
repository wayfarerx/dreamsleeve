/*
 * DocumentsSpec.scala
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

import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits._

import org.scalatest._

/**
 * Test case for the document binary codec.
 */
class DocumentsSpec extends FlatSpec with Matchers {

  "A document" should "encode to and decode from binary" in {
    val d = Document("hello", Value.Boolean(true))
    val codec = Codec[Document]
    codec.encode(d) shouldBe
      Attempt.successful(bin"0000000000000101" ++ BitVector("hello".getBytes(StandardCharsets.UTF_8)) ++ bin"001")
    codec.decode(bin"0000000000000101" ++ BitVector("hello".getBytes(StandardCharsets.UTF_8)) ++ bin"001") shouldBe
      Attempt.successful(DecodeResult(d, BitVector.empty))
  }

}
