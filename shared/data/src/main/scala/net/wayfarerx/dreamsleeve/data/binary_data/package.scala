/*
 * package.scala
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

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

/**
 * Global definitions for the binary data package.
 */
package object binary_data {

  /** The implicit hash codec. */
  implicit val HashCodec: Codec[Hash] = bytes(Hash.Size).xmap(
    b => Hash.setInternalRepresentation(b.toArray),
    h => ByteVector(Hash.getInternalRepresentation(h))
  )

}
