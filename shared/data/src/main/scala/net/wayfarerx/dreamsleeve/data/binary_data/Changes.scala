/*
 * Changes.scala
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

import scodec.Codec
import scodec.codecs._

/**
 * Binary support for the change factory object.
 */
trait Changes {

  /** The marker denoting that change is discriminated. */
  @inline
  final implicit def binaryDiscriminated: Discriminated[Change, Int] = Changes.Discriminated

  /** The implicit change codec. */
  @inline
  final implicit def binaryCodec: Codec[Change] = Changes.Codec

}

/**
 * Support for binary change codecs.
 */
object Changes {

  /** The marker denoting that change is discriminated. */
  val Discriminated: Discriminated[Change, Int] = scodec.codecs.Discriminated(uint(3))

  /** The change codec. */
  val Codec: Codec[Change] =
    (Copies.Codec :+: Replaces.Codec :+: Modifies.Codec :+: Adds.Codec :+: Removes.Codec).as[Change].auto

}
