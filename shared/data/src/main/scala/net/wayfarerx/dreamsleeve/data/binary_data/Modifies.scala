/*
 * Modifies.scala
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
 * Binary support for the modify factory object.
 */
trait Modifies {

  /** The implicit change discriminator for modifies. */
  @inline
  final implicit def binaryAsChange: Discriminator[Change, Update.Modify, Int] = Modifies.AsChange

  /** The implicit update discriminator for modifies. */
  @inline
  final implicit def binaryAsUpdate: Discriminator[Update, Update.Modify, Int] = Modifies.AsUpdate

  /** The implicit modify codec. */
  @inline
  final implicit def binaryCodec: Codec[Update.Modify] = Modifies.Codec

}

/**
 * Support for binary modify codecs.
 */
object Modifies {

  /** The change discriminator for modifies. */
  val AsChange: Discriminator[Change, Update.Modify, Int] = Discriminator(3)

  /** The update discriminator for modifies. */
  val AsUpdate: Discriminator[Update, Update.Modify, Int] = Discriminator(AsChange.value)

  /** The modify codec. */
  val Codec: Codec[Update.Modify] = lazily((HashCodec ~ vectorOfN(uint16, Values.Codec ~ Changes.Codec))
    .xmap(p => Update.Modify(p._1, p._2: _*), m => m.fromHash -> m.changes.toVector))

}
