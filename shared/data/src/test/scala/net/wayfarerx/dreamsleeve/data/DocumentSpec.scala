/*
 * DocumentSpec.scala
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

import org.scalatest._

/**
 * Test case for the document implementation.
 */
class DocumentSpec extends FlatSpec with Matchers {

  "A document" should "act as a hashable root fragment container" in {
    val e = Document("e", Table())
    val o = Document("o", Table(Value.Number(1) -> Value.String("1")))
    e.hash shouldBe Hasher()(Document.Header, "e", e.content.hash)
    o.hash shouldBe Hasher()(Document.Header, "o", o.content.hash)
    Document.unapply(o) shouldBe Some((o.title, o.content))
  }

}
