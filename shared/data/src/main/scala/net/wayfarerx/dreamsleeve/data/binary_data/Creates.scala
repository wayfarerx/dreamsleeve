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
 * Binary support for the create factory object.
 */
trait Creates {

  /** The implicit difference discriminator for creates. */
  @inline
  final implicit def binaryAsDifference: Discriminator[Difference, Difference.Create, Int] = Creates.AsDifference

  /** The implicit create codec. */
  @inline
  final implicit def binaryCodec: Codec[Difference.Create] = Creates.Codec

}

/**
 * Support for binary create codecs.
 */
object Creates {

  /** The difference discriminator for create. */
  val AsDifference: Discriminator[Difference, Difference.Create, Int] = Discriminator(1)

  /** The create codec. */
  val Codec: Codec[Difference.Create] = Documents.Codec.as[Difference.Create]

}
