/*
 * Revises.scala
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
 * Binary support for the revise factory object.
 */
trait Revises {

  /** The implicit difference discriminator for revises. */
  @inline
  final implicit def binaryAsDifference: Discriminator[Difference, Difference.Revise, Int] = Revises.AsDifference

  /** The implicit revise codec. */
  @inline
  final implicit def binaryCodec: Codec[Difference.Revise] = Revises.Codec

}

/**
 * Support for binary revise codecs.
 */
object Revises {

  /** The difference discriminator for revises. */
  val AsDifference: Discriminator[Difference, Difference.Revise, Int] = Discriminator(2)

  /** The revise codec. */
  val Codec: Codec[Difference.Revise] =
    (HashCodec :: variableSizeBytes(uint16, utf8) :: Updates.Codec).as[Difference.Revise]

}
