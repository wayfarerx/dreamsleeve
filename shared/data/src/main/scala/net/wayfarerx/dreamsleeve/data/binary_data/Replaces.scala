/*
 * Replaces.scala
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
 * Binary support for the replace factory object.
 */
trait Replaces {

  /** The implicit change discriminator for replaces. */
  @inline
  final implicit def binaryAsChange: Discriminator[Change, Update.Replace, Int] = Replaces.AsChange

  /** The implicit update discriminator for replaces. */
  @inline
  final implicit def binaryAsUpdate: Discriminator[Update, Update.Replace, Int] = Replaces.AsUpdate

  /** The implicit replace codec. */
  @inline
  final implicit def binaryCodec: Codec[Update.Replace] = Replaces.Codec

}

/**
 * Support for binary replace codecs.
 */
object Replaces {

  /** The change discriminator for replaces. */
  val AsChange: Discriminator[Change, Update.Replace, Int] = Discriminator(2)

  /** The update discriminator for replaces. */
  val AsUpdate: Discriminator[Update, Update.Replace, Int] = Discriminator(AsChange.value)

  /** The replace codec. */
  val Codec: Codec[Update.Replace] = (HashCodec :: Fragments.Codec).as[Update.Replace]

}
