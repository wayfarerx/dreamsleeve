/*
 * Adds.scala
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
 * Binary support for the add factory object.
 */
trait Adds {

  /** The implicit change discriminator for adds. */
  @inline
  final implicit def binaryAsChange: Discriminator[Change, Change.Add, Int] = Adds.AsChange

  /** The implicit add codec. */
  @inline
  final implicit def binaryCodec: Codec[Change.Add] = Adds.Codec

}

/**
 * Support for binary add codecs.
 */
object Adds {

  /** The change discriminator for adds. */
  val AsChange: Discriminator[Change, Change.Add, Int] = Discriminator(4)

  /** The add codec. */
  val Codec: Codec[Change.Add] = Fragments.Codec.as[Change.Add]

}
